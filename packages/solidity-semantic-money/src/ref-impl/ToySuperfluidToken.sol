// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

// solhint-disable not-rely-on-time

import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
import { Clones } from "@openzeppelin/contracts/proxy/Clones.sol";
import { FlowId, ISuperfluidToken } from "./ISuperfluidToken.sol";
import {
    Time, Value, FlowRate, Unit,
    BasicParticle,
    PDPoolIndex, PDPoolMember, PDPoolMemberMU
} from "../SemanticMoney.sol";
import { TokenMonad } from "../TokenMonad.sol";
import {
    ISuperfluidPool, ToySuperfluidPool
} from "./ToySuperfluidPool.sol";


/**
 * @dev A very special toy super token implementation.
 *
 * Features:
 * - Pure super token, no ERC20 wrapping business.
 * - Negative account is allowed.
 * - no permission control for account going negative.
 */
contract ToySuperfluidToken is ISuperfluidToken, TokenMonad {
    using EnumerableSet for EnumerableSet.AddressSet;

    ToySuperfluidPool public immutable POOL_CONTRACT_MASTER_COPY;

    Time public LIQUIDATION_PERIOD = Time.wrap(42 minutes);

    struct AccountData {
        Value    totalBuffer;
        FlowRate totalInflowRate;
        FlowRate totalOutflowRate;
    }

    struct FlowData {
        address  from;
        address  to;
        FlowRate flowRate;
        Value    buffer;
    }

    mapping (address owner => BasicParticle) public uIndexes;
    mapping (address owner => AccountData) public accountData;
    mapping (bytes32 flowHash => FlowData) public flowData;
    mapping (address pool => bool exist) private _poolExistenceFlags;
    mapping (address owner => EnumerableSet.AddressSet poolConnections) private _poolConnectionsMap;

    constructor () {
        POOL_CONTRACT_MASTER_COPY = new ToySuperfluidPool();
    }

    function setLiquidationPeriod(Time a) external {
        LIQUIDATION_PERIOD = a;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives
    ////////////////////////////////////////////////////////////////////////////////

    function realtimeBalanceOf(address account) override external view
        returns (Value rtb)
    {
        Time t = Time.wrap(uint32(block.timestamp));
        return realtimeBalanceAt(account, t);
    }

    function realtimeBalanceAt(address account, Time t) override public view
        returns (Value rtb)
    {
        (Value available, ) = realtimeBalanceVectorAt(account, t);
        rtb = available;
    }

    function realtimeBalanceVectorAt(address account, Time t) override public view
        returns (Value available, Value deposit)
    {
        // initial value from universal index
        available = _getUIndex(new bytes(0), account).rtb(t);

        // pending distributions from pool
        if (_isPool(account)) {
            // NB! Please ask solidity designer why "+=" is not derived for overloaded operator custom types
            available = available + ISuperfluidPool(account).getPendingDistribution();
        }

        // pool-connected balance
        {
            EnumerableSet.AddressSet storage connections = _poolConnectionsMap[account];
            for (uint i = 0; i < connections.length(); ++i) {
                ISuperfluidPool p = ISuperfluidPool(connections.at(i));
                available = available + p.getClaimable(t, account);
            }
        }

        deposit = accountData[account].totalBuffer;
    }

    function getNetFlowRate(address account) override external view returns (FlowRate nr) {
        nr = _getUIndex(new bytes(0), account).flow_rate();

        // pool distribution flow rate
        if (_isPool(account)) {
            nr = nr + ISuperfluidPool(account).getPendingDistributionFlowRate();
        }

        // pool-connected flows
        {
            EnumerableSet.AddressSet storage connections = _poolConnectionsMap[account];
            for (uint i = 0; i < connections.length(); ++i) {
                ISuperfluidPool p = ISuperfluidPool(connections.at(i));
                nr = nr + p.getMemberFlowRate(account);
            }
        }
    }

    function getFlowHash(address from, address to, FlowId flowId) public view returns (bytes32) {
        return keccak256(abi.encode(block.chainid, "flow", from, to, flowId));
    }

    function getDistributionFlowHash(address from, ISuperfluidPool to, FlowId flowId) public view returns (bytes32) {
        return keccak256(abi.encode(block.chainid, "distributionflow", from, address(to), flowId));
    }

    // getTotalFlowRate(address from, address to)
    function getFlowRate(address from, address to, FlowId flowId) override external view returns (FlowRate) {
        return _getFlowRate(new bytes(0), getFlowHash(from, to, flowId));
    }

    // This is the non-ERC20 version of instant transfer, that can trigger actions defined by "to"
    function _shift(bytes memory eff, address operator, address from, address to, Value amount) internal
        returns (bool success)
    {
        /// check inputs
        require(!_isPool(to), "Use distribute to pool");
        require(Value.unwrap(amount) >= 0, "Amount must not be negative");

        // permission control
        require(_acl(operator, from, to, amount, FlowRate.wrap(0)), "ACL for shift not supported");

        // Make updates
        _doShift(eff, from, to, amount);
        return true;
    }

    function shift(address from, address to, Value amount)
        virtual override external returns (bool success)
    {
        return _shift(new bytes(0), msg.sender, from, to, amount);
    }

    function _flow(bytes memory eff, address operator, address from, address to, FlowId flowId, FlowRate flowRate)
        internal returns (bool success)
    {
        /// check inputs
        require(!_isPool(to), "Use distributeFlow to pool");
        require(FlowRate.unwrap(flowRate) >= 0, "Flow rate must not be negative");
        require(from != to, "Shall not send flow to oneself");

        // permission control
        require(_acl(operator, from, to, Value.wrap(0), flowRate), "ACL for flow not supported");

        /// prepare local variables (let bindings)
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowHash = getFlowHash(from, to, flowId);
        FlowRate oldFlowRate = _getFlowRate(new bytes(0), flowHash);

        // Make updates
        eff = _doFlow(eff, from, to, flowHash, flowRate, t);
        eff = _adjustBuffer(eff, from, flowHash, oldFlowRate, flowRate /* assert(newFlowRate == flowRate) */);

        return true;
    }

    function flow(address from, address to, FlowId flowId, FlowRate flowRate)
        virtual override external returns (bool success)
    {
        return _flow(new bytes(0), msg.sender, from, to, flowId, flowRate);
    }

    function _distribute(bytes memory eff, address operator, address from, ISuperfluidPool to, Value reqAmount)
        internal returns (bool success, Value actualAmount)
    {
        /// check inputs
        require(_isPool(address(to)), "Distribute to pool only");
        require(Value.unwrap(reqAmount) >= 0, "Requested amount must not be negative");

        // permission control
        // FIXME actualAmount
        require(_acl(operator, from, address(to), reqAmount, FlowRate.wrap(0)), "ACL for distribute not supported");

        // Make updates
        (, actualAmount) = _doDistribute(eff, from, address(to), reqAmount);
        success = true;
    }

    function distribute(address from, ISuperfluidPool to, Value reqAmount)
        virtual override external returns (bool success, Value actualAmount)
    {
        return _distribute(new bytes(0), msg.sender, from, to, reqAmount);
    }

    function _distributeFlow(bytes memory eff, address operator, address from, ISuperfluidPool to,
                             FlowId flowId, FlowRate reqFlowRate)
        internal returns (bool success, FlowRate actualFlowRate, FlowRate newDistributionFlowRate)
    {
        /// check inputs
        require(_isPool(address(to)), "Distribute flow to pool only");
        require(FlowRate.unwrap(reqFlowRate) >= 0, "Requested flow rate must not be negative");

        /// prepare local variables
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowHash = getDistributionFlowHash(from, to, flowId);
        FlowRate oldFlowRate = _getFlowRate(new bytes(0), flowHash);

        // permission control
        // FIXME actualFlowRate
        require(_acl(operator, from, address(to), Value.wrap(0), reqFlowRate), "ACL for distribute not supported");

        // Make updates
        (eff, actualFlowRate, newDistributionFlowRate) = _doDistributeFlow
            (eff, from, address(to), flowHash, reqFlowRate, t);
        eff = _adjustBuffer(eff, from, flowHash, oldFlowRate, actualFlowRate);
        success = true;
    }

    function distributeFlow(address from, ISuperfluidPool to, FlowId flowId, FlowRate reqFlowRate)
        virtual override external returns (bool success, FlowRate actualFlowRate, FlowRate newDistributionFlowRate)
    {
        return _distributeFlow(new bytes(0), msg.sender, from, to, flowId, reqFlowRate);
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    function createPool() external
        returns (ToySuperfluidPool pool)
    {
        pool = ToySuperfluidPool(Clones.clone(address(POOL_CONTRACT_MASTER_COPY)));
        pool.initialize(msg.sender);
        _registerPool(address(pool));
    }

    function isPool(address p) override external view returns (bool) {
        return _isPool(p);
    }

    function getNumConnections(address account) override external view returns (uint) {
        return _poolConnectionsMap[account].length();
    }

    function connectPool(ISuperfluidPool to) override external
        returns (bool success) {
        return connectPool(to, true);
    }

    function disconnectPool(ISuperfluidPool to) override external
        returns (bool success) {
        return connectPool(to, false);
    }

    function connectPool(ISuperfluidPool to, bool doConnect) override public
        returns (bool success)
    {
        require(_isPool(address(to)), "Not a pool!!");

        Time t = Time.wrap(uint32(block.timestamp));
        if (doConnect) {
            if (!_poolConnectionsMap[msg.sender].contains(address(to))) {
                _poolConnectionsMap[msg.sender].add(address(to));
                assert(to.operatorConnectMember(t, msg.sender, true));
            }
        } else {
            if (_poolConnectionsMap[msg.sender].contains(address(to))) {
                _poolConnectionsMap[msg.sender].remove(address(to));
                assert(to.operatorConnectMember(t, msg.sender, false));
            }
        }
        return true;
    }

    function isMemberConnected(ISuperfluidPool to, address memberAddr) override external view returns (bool) {
        return _poolConnectionsMap[memberAddr].contains(address(to));
    }

    /// This is used by the pool to adjust flow rate
    function _absorbParticlesFromPool(bytes memory eff, address[] calldata accounts, BasicParticle[] calldata ps)
        internal returns (bool)
    {
        require(_isPool(msg.sender), "Only absorbing from pools!");
        assert(accounts.length == ps.length);
        for (uint i = 0; i < accounts.length; i++) {
            _setUIndex(eff, accounts[i], _getUIndex(eff, accounts[i]).mappend(ps[i]));
        }
        return true;
    }

    function absorbParticlesFromPool(address[] calldata accounts, BasicParticle[] calldata ps)
        virtual override external returns (bool)
    {
        return _absorbParticlesFromPool(new bytes(0), accounts, ps);
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Other Internal Functions
    ////////////////////////////////////////////////////////////////////////////////

    function _isPool(address pool) internal view virtual returns (bool) {
        return _poolExistenceFlags[pool];
    }
    function _registerPool(address pool) internal virtual {
        _poolExistenceFlags[pool] = true;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // ACL Hook
    ////////////////////////////////////////////////////////////////////////////////

    function _acl(address operator, address from, address /* to */,
                  Value /* shiftAmount */, FlowRate /* shiftFlowRate */)
        virtual internal returns (bool)
    {
        return operator == from /* || to != address(this) */;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Buffer Based Solvency
    ////////////////////////////////////////////////////////////////////////////////

    function _adjustBuffer(bytes memory eff,
                           address /* from */, bytes32 /* flowHash */,
                           FlowRate /* oldFlowRate */, FlowRate /* newFlowRate */)
        virtual internal returns (bytes memory)
    {
        return eff;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Token Monad Overrides
    ////////////////////////////////////////////////////////////////////////////////

    function _getUIndex(bytes memory /*eff*/, address owner)
        virtual override internal view returns (BasicParticle memory)
    {
        return uIndexes[owner];
    }
    function _setUIndex(bytes memory eff, address owner, BasicParticle memory p)
        virtual override internal returns (bytes memory)
    {
        uIndexes[owner] = p;
        return eff;
    }

    function _getPDPIndex(bytes memory /*eff*/, address pool)
        virtual override internal view returns (PDPoolIndex memory)
    {
        return ISuperfluidPool(pool).getIndex();
    }
    function _setPDPIndex(bytes memory eff, address pool, PDPoolIndex memory p)
        virtual override internal returns (bytes memory)
    {
        assert(ISuperfluidPool(pool).operatorSetIndex(p));
        return eff;
    }

    function _getFlowRate(bytes memory /*eff*/, bytes32 flowHash)
        virtual override internal view returns (FlowRate)
    {
        return flowData[flowHash].flowRate;
    }
    function _setFlowInfo(bytes memory eff, bytes32 flowHash, address from, address to,
                          FlowRate newFlowRate, FlowRate flowRateDelta)
        virtual override internal returns (bytes memory)
    {
        // the buffer amount now mismatches the newFlowRate, it should be adjusted separately.
        flowData[flowHash] = FlowData(from, to, newFlowRate, flowData[flowHash].buffer);
        accountData[from].totalOutflowRate = accountData[from].totalOutflowRate + flowRateDelta;
        accountData[to].totalInflowRate = accountData[from].totalInflowRate + flowRateDelta;
        return eff;
    }
}
