// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

// solhint-disable not-rely-on-time

import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import { Clones } from "@openzeppelin/contracts/proxy/Clones.sol";
import { FlowId, ISuperToken } from "./ISuperToken.sol";
import {
    Time, Value, FlowRate, Unit,
    BasicParticle,
    PDPoolIndex, PDPoolMember, PDPoolMemberMU
} from "../SemanticMoney.sol";
import { TokenMonad } from "../TokenMonad.sol";
import {
    ISuperTokenPool, ToySuperTokenPool
} from "./ToySuperTokenPool.sol";

/**
 * @dev A very special toy super token implementation.
 *
 * Features:
 * - Pure super token, no ERC20 wrapping business.
 * - Negative account is allowed.
 * - no permission control for account going negative.
 */
contract ToySuperToken is ISuperToken, TokenMonad {
    using EnumerableSet for EnumerableSet.AddressSet;

    ToySuperTokenPool public immutable POOL_CONTRACT_MASTER_COPY;

    mapping (address owner => BasicParticle) public uIndexes;
    mapping (bytes32 flowHash => FlowRate) public flowRates;
    mapping (address pool => bool exist) private _poolExistenceFlags;
    mapping (address owner => EnumerableSet.AddressSet poolConnections) private _poolConnectionsMap;
    mapping (address owner => mapping(address => uint256) allowances) private _allowances;

    constructor () {
        POOL_CONTRACT_MASTER_COPY = new ToySuperTokenPool();
    }

    ////////////////////////////////////////////////////////////////////////////////
    // ERC20 operations
    ////////////////////////////////////////////////////////////////////////////////

    function totalSupply() external pure returns (uint256) {
        // Yes, I mean it.
        return 0;
    }

    function balanceOf(address account) override external view returns (uint256) {
        Time t = Time.wrap(uint32(block.timestamp));
        (Value avb, ) = realtimeBalanceVectorAt(account, t);
        return Value.unwrap(avb) > 0 ? uint256(Value.unwrap(avb)) : 0;
    }

    function transfer(address to, uint256 amount) override external returns (bool) {
        address owner = msg.sender;
        _shift(owner, to, Value.wrap(SafeCast.toInt256(amount)), false);
        return true;
    }

    function transferFrom(address from, address to, uint256 amount) override external returns (bool) {
        return _shift(from, to, Value.wrap(SafeCast.toInt256(amount)), true);
    }

    function allowance(address owner, address spender) override external view returns (uint256) {
        return _allowances[owner][spender];
    }

    function approve(address spender, uint256 amount) override external returns (bool) {
        address owner = msg.sender;
        _approve(owner, spender, amount);
        return true;
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
        (Value available, Value deposit) = realtimeBalanceVectorAt(account, t);
        rtb = available - deposit;
    }

    function realtimeBalanceVectorAt(address account, Time t) override public view
        returns (Value available, Value deposit)
    {
        // initial value from universal index
        available = _getUIndex(new bytes(0), account).rtb(t);

        // pending distributions from pool
        if (_isPool(account)) {
            // NB! Please ask solidity designer why "+=" is not derived for overloaded operator custom types
            available = available + ISuperTokenPool(account).getPendingDistribution();
        }

        // pool-connected balance
        {
            EnumerableSet.AddressSet storage connections = _poolConnectionsMap[account];
            for (uint i = 0; i < connections.length(); ++i) {
                ISuperTokenPool p = ISuperTokenPool(connections.at(i));
                available = available + p.getClaimable(t, account);
            }
        }

        // TODO: buffer based solvency
        deposit = Value.wrap(0);
    }

    function getNetFlowRate(address account) override external view returns (FlowRate nr)
    {
        nr = _getUIndex(new bytes(0), account).flow_rate;

        // pool distribution flow rate
        if (_isPool(account)) {
            nr = nr + ISuperTokenPool(account).getPendingDistributionFlowRate();
        }

        // pool-connected flows
        {
            EnumerableSet.AddressSet storage connections = _poolConnectionsMap[account];
            for (uint i = 0; i < connections.length(); ++i) {
                ISuperTokenPool p = ISuperTokenPool(connections.at(i));
                nr = nr + p.getMemberFlowRate(account);
            }
        }
    }

    function getFlowHash(address from, address to, FlowId flowId) public view returns (bytes32) {
        return keccak256(abi.encode(block.chainid, "flow", from, to, flowId));
    }

    function getDistributionFlowHash(address from, ISuperTokenPool to, FlowId flowId) public view returns (bytes32) {
        return keccak256(abi.encode(block.chainid, "distributionflow", from, address(to), flowId));
    }

    function getFlowRate(address from, address to, FlowId flowId) override external view returns (FlowRate)
    {
        return _getFlowRate(new bytes(0), getFlowHash(from, to, flowId));
    }

    function _shift(address from, address to, Value amount, bool checkAllowance) internal
        returns (bool success)
    {
        /// check inputs
        require(!_isPool(to), "Use distribute to pool");
        require(Value.unwrap(amount) >= 0, "Amount must not be negative");

        /// prepare local variables (let bindings)
        address spender = msg.sender;
        // NOTE: uint256 casting is safe: amount is required to be non negative
        if (checkAllowance) _spendAllowance(from, spender, uint256(Value.unwrap(amount)));

        // Make updates
        _doShift(new bytes(0), from, to, amount);
        return true;
    }

    // This is the non-ERC20 version of instant transfer, that can trigger actions defined by "to"
    function shift(address from, address to, Value amount) override external
        returns (bool success)
    {
        return _shift(from, to, amount, from != to);
    }

    function flow(address from, address to, FlowId flowId, FlowRate flowRate) public
        returns (bool success)
    {
        /// check inputs
        require(!_isPool(to), "Use distributeFlow to pool");
        require(FlowRate.unwrap(flowRate) >= 0, "Flow rate must not be negative");
        require(from != to, "Shall not send flow to oneself");

        // TODO: plug permission controls
        require(msg.sender == from, "No flow permission");

        /// prepare local variables (let bindings)
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowHash = getFlowHash(from, to, flowId);

        // Make updates
        _doFlow(new bytes(0), from, to, flowHash, flowRate, t);
        return true;
    }

    function distribute(address from, ISuperTokenPool to, Value reqAmount) override public
        returns (bool success, Value actualAmount)
    {
        /// check inputs
        require(_isPool(address(to)), "Distribute to pool only");
        require(Value.unwrap(reqAmount) >= 0, "Requested amount must not be negative");

        // TODO: plug permission controls
        require(msg.sender == from, "No distribute permission");

        // Make updates
        actualAmount = _doDistribute(new bytes(0), from, address(to), reqAmount);
        success = true;
    }

    function distributeFlow(address from, ISuperTokenPool to, FlowId flowId, FlowRate reqFlowRate) override public
        returns (bool success, FlowRate actualFlowRate)
    {
        /// check inputs
        require(_isPool(address(to)), "Distribute flow to pool only");
        require(FlowRate.unwrap(reqFlowRate) >= 0, "Requested flow rate must not be negative");

        /// prepare local variables
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowHash = getDistributionFlowHash(from, to, flowId);

        // TODO: plug permission controls
        require(msg.sender == from, "No distribute flow permission");

        // Make updates
        actualFlowRate = _doDistributeFlow(new bytes(0), from, address(to), flowHash, reqFlowRate, t);
        success = true;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    function isPool(address p) external view returns (bool) {
        return _isPool(p);
    }

    function getNumConnections(address account) override external view returns (uint) {
        return _poolConnectionsMap[account].length();
    }

    function createPool() external
        returns (ToySuperTokenPool pool)
    {
        pool = ToySuperTokenPool(Clones.clone(address(POOL_CONTRACT_MASTER_COPY)));
        pool.initialize(msg.sender);
        _registerPool(address(pool));
    }

    function connectPool(ISuperTokenPool to) override external
        returns (bool success) {
        return connectPool(to, true);
    }

    function disconnectPool(ISuperTokenPool to) override external
        returns (bool success) {
        return connectPool(to, false);
    }

    function connectPool(ISuperTokenPool to, bool doConnect) override public
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

    function isMemberConnected(ISuperTokenPool to, address memberAddr) override external view returns (bool) {
        return _poolConnectionsMap[memberAddr].contains(address(to));
    }

    /// This is used by the pool to adjust flow rate
    function absorbParticlesFromPool(address[] calldata accounts, BasicParticle[] calldata ps) override external
        returns (bool)
    {
        require(_isPool(msg.sender), "Only absorbing from pools!");
        assert(accounts.length == ps.length);
        for (uint i = 0; i < accounts.length; i++) {
            _setUIndex(new bytes(0), accounts[i], _getUIndex(new bytes(0), accounts[i]).mappend(ps[i]));
        }
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // ERC20-style approval/allowance System for shift2
    ////////////////////////////////////////////////////////////////////////////////

    /**
     * @dev Sets `amount` as the allowance of `spender` over the `owner` s tokens.
     *
     * This internal function is equivalent to `approve`, and can be used to
     * e.g. set automatic allowances for certain subsystems, etc.
     *
     * Emits an {Approval} event.
     *
     * Requirements:
     *
     * - `owner` cannot be the zero address.
     * - `spender` cannot be the zero address.
     */
    function _approve(
        address owner,
        address spender,
        uint256 amount
    ) internal virtual {
        require(owner != address(0), "ERC20: approve from the zero address");
        require(spender != address(0), "ERC20: approve to the zero address");

        _allowances[owner][spender] = amount;
        emit Approval(owner, spender, amount);
    }

    /**
     * @dev Updates `owner` s allowance for `spender` based on spent `amount`.
     *
     * Does not update the allowance amount in case of infinite allowance.
     * Revert if not enough allowance is available.
     *
     * Might emit an {Approval} event.
     */
    function _spendAllowance(
        address owner,
        address spender,
        uint256 amount
    ) internal virtual {
        uint256 currentAllowance = _allowances[owner][spender];
        if (currentAllowance != type(uint256).max) {
            require(currentAllowance >= amount, "ERC20: insufficient allowance");
            unchecked {
                _approve(owner, spender, currentAllowance - amount);
            }
        }
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
    // Token Monad Overrides
    ////////////////////////////////////////////////////////////////////////////////

    function _getUIndex(bytes memory /*eff*/, address owner)
        internal view virtual override returns (BasicParticle memory)
    {
        return uIndexes[owner];
    }
    function _setUIndex(bytes memory /*eff*/, address owner, BasicParticle memory p)
        internal virtual override
    {
        uIndexes[owner] = p;
    }
    function _getPDPIndex(bytes memory /*eff*/, address pool)
        internal view virtual override returns (PDPoolIndex memory)
    {
        return ISuperTokenPool(pool).getIndex();
    }
    function _setPDPIndex(bytes memory /*eff*/, address pool, PDPoolIndex memory p)
        internal virtual override
    {
        assert(ISuperTokenPool(pool).operatorSetIndex(p));
    }
    function _getFlowRate(bytes memory /*eff*/, bytes32 flowHash)
        internal view virtual override returns (FlowRate)
    {
        return flowRates[flowHash];
    }
    function _setFlowInfo(bytes memory /*eff*/, bytes32 flowHash, address /*from*/, address /*to*/, FlowRate flowRate)
        internal virtual override
    {
        flowRates[flowHash] = flowRate;
    }
}
