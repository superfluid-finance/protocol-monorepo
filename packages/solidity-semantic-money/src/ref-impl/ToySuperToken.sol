// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

// solhint-disable not-rely-on-time

import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
import { FlowId, ISuperToken } from "./ISuperToken.sol";
import {
    Time, Value, FlowRate, Unit,
    BasicParticle, mempty_basic_particle,
    PDPoolIndex, PDPoolMember, PDPoolMemberMU
} from "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {
    ISuperTokenPool, ToySuperTokenPool
} from "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperTokenPool.sol";


/**
 * @dev A very special toy super token implementation.
 *
 * Features:
 * - Pure super token, no ERC20 wrapping business.
 * - Negative account is allowed.
 * - no permission control for account going negative.
 */
contract ToySuperToken is ISuperToken {
    using EnumerableSet for EnumerableSet.AddressSet;

    mapping (address owner => BasicParticle) public uIndexes;
    mapping (bytes32 flowHash => FlowRate) public flowRates;
    EnumerableSet.AddressSet internal _pools;
    mapping (address owner => EnumerableSet.AddressSet poolConnections) private _poolConnectionsMap;
    mapping (address owner => mapping(address => uint256) allowances) private _allowances;

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
        _shift(owner, to, Value.wrap(int256(amount)), false); // FIXME safeCast
        return true;
    }

    function transferFrom(address from, address to, uint256 amount) override external returns (bool) {
        return _shift(from, to, Value.wrap(int256(amount)), true); // FIXME safeCast
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
    //
    // FIXME require(from != to), as honeyport for F/V
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
        available = uIndexes[account].rtb(t);

        // pending distributions from pool
        if (_pools.contains(account)) {
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
        nr = uIndexes[account].flow_rate;

        // pool distribution flow rate
        if (_pools.contains(account)) {
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
        return flowRates[getFlowHash(from, to, flowId)];
    }

    function _shift(address from, address to, Value amount, bool checkAllowance) internal
        returns (bool success)
    {
        require(!_pools.contains(to), "Is a pool!");
        require(Value.unwrap(amount) >= 0, "Negative amount!");
        address spender = msg.sender;
        if (checkAllowance) _spendAllowance(from, spender, uint256(Value.unwrap(amount))); // FIXME SafeCast
        // Make updates
        (uIndexes[from], uIndexes[to]) = uIndexes[from].shift2(uIndexes[to], amount);
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
        require(from != to, "No blue elephant!");
        require(!_pools.contains(to), "Is a pool!");
        require(FlowRate.unwrap(flowRate) >= 0, "Negative flow rate!");

        // FIXME: plug permission controls
        require(msg.sender == from, "No flow permission!");

        /// prepare local variables (let bindings)
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowHash = getFlowHash(from, to, flowId);

        // Make updates
        FlowRate flowRateDelta = flowRate - flowRates[flowHash];
        (uIndexes[from], uIndexes[to]) = uIndexes[from].shift_flow2a(uIndexes[to], flowRateDelta, t);
        flowRates[flowHash] = flowRate;
        return true;
    }

    function distribute(address from, ISuperTokenPool to, Value reqAmount) override public
        returns (bool success, Value actualAmount)
    {
        /// check inputs
        require(_pools.contains(address(to)), "Not a pool!");
        require(Value.unwrap(reqAmount) >= 0, "Negative amount not allowed!!");

        // FIXME: plug permission controls
        require(msg.sender == from, "No distribute flow permission!");

        // Make updates
        PDPoolIndex memory pdidx = to.getIndex();
        (uIndexes[from], pdidx, actualAmount) = uIndexes[from].shift2(pdidx, reqAmount);
        assert(to.operatorSetIndex(pdidx));
        success = true;
    }

    function distributeFlow(address from, ISuperTokenPool to, FlowId flowId, FlowRate reqFlowRate) override public
        returns (bool success, FlowRate actualFlowRate)
    {
        /// check inputs
        require(_pools.contains(address(to)), "Not a pool!!");
        require(FlowRate.unwrap(reqFlowRate) >= 0, "Negative flow rate not allowed!!");

        /// prepare local variables
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowHash = getDistributionFlowHash(from, to, flowId);

        // FIXME: plug permission controls
        require(msg.sender == from, "No flow permission!!");

        // Make updates
        PDPoolIndex memory pdidx = to.getIndex();
        (uIndexes[from], pdidx, actualFlowRate) = uIndexes[from].shift_flow2b
            (pdidx, reqFlowRate - flowRates[flowHash], t);
        to.operatorSetIndex(pdidx);
        flowRates[flowHash] = actualFlowRate;
        success = true;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    function isPool(address p) external view returns (bool)
    {
        return _pools.contains(p);
    }

    function createPool() external
        returns (ToySuperTokenPool pool)
    {
        pool = new ToySuperTokenPool(msg.sender);
        _pools.add(address(pool));
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
        require(_pools.contains(address(to)), "Not a pool!!");

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

    function getNumConnections(address account) override external view returns (uint) {
        return _poolConnectionsMap[account].length();
    }

    /// This is used by the pool to adjust flow rate
    function absorbParticlesFromPool(address[] calldata accounts, BasicParticle[] calldata ps) override external
        returns (bool)
    {
        require(_pools.contains(msg.sender), "Only absorbing from pools!");
        assert(accounts.length == ps.length);
        for (uint i = 0; i < accounts.length; i++) {
            uIndexes[accounts[i]] = uIndexes[accounts[i]].mappend(ps[i]);
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

}
