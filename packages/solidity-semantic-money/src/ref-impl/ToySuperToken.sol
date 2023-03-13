// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import "./ISuperToken.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";

/**
 * @dev A toy implementation for proportional distribution pool.
 *
 * NOTE: Solidity public getter function for the storage fields do not support structs,
 *       hence their public getter are added manually instead.
 */
contract ToySuperTokenPool is Ownable, ISuperTokenPool {
    PDPoolIndex internal _index;
    address public admin;
    mapping (address member => PDPoolMember member_data) internal _members;
    mapping (address member => Value claimed_value) internal _claimedValues;
    Unit public pendingUnits;

    constructor (address admin_)
        Ownable()
    {
        admin = admin_;
    }

    function getPendingDistribution() external view
        returns (Value)
    {
        Time t = Time.wrap(uint32(block.timestamp));
        return _index.wrapped_particle.rtb(t).mul(pendingUnits);
    }

    function getIndex() override external view
        returns (PDPoolIndex memory)
    {
        return _index;
    }

    function updateMember(address memberAddr, Unit unit) external
        returns (bool)
    {
        require(Unit.unwrap(unit) >= 0, "Negative unit number not supported");
        require(admin == msg.sender, "Not pool admin");
        Time t = Time.wrap(uint32(block.timestamp));

        // update pool's pending units
        if (!ISuperToken(owner()).isMemberConnected(this, memberAddr)) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units + unit;
        }

        // update pool member's units
        BasicParticle memory p;
        (_index, _members[memberAddr], p) = PDPoolMemberMU(_index, _members[memberAddr])
            .pool_member_update(p, unit, t);
        assert(ISuperToken(owner()).absorbParticleFromPool(admin, p));

        // additional side effects of triggering claimAll
        claimAll(t, memberAddr);

        return true;
    }

    function getClaimable(Time t, address memberAddr) override public view
        returns (Value)
    {
        return PDPoolMemberMU(_index, _members[memberAddr]).rtb(t) - _claimedValues[memberAddr];
    }

    function claimAll(Time t, address memberAddr) override public returns (bool) {
        Value c = getClaimable(t, memberAddr);
        assert(ISuperToken(owner()).shift(address(this), memberAddr, c));
        _claimedValues[memberAddr] = c;
        return true;
    }

    function claimAll() external returns (bool) {
        Time t = Time.wrap(uint32(block.timestamp));
        return claimAll(t, msg.sender);
    }

    function operatorSetIndex(PDPoolIndex calldata index) override external
        onlyOwner returns (bool)
    {
        _index = index;
        return true;
    }

    function operatorConnectMember(Time t, address memberAddr, bool doConnect) override external
        onlyOwner returns (bool)
    {
        if (doConnect) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units;
        } else {
            pendingUnits = pendingUnits + _members[memberAddr].owned_units;
        }
        // additional side effects of triggering claimAll
        claimAll(t, memberAddr);
        return true;
    }
}

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
    mapping (bytes32 flowAddress => FlowRate) public flowRates;
    mapping (ISuperTokenPool pool => bool exist) public pools;
    mapping (address owner => EnumerableSet.AddressSet) internal _connectionsMap;
    mapping (address => mapping(address => uint256)) private _allowances;

    ////////////////////////////////////////////////////////////////////////////////
    // ERC20 operations
    ////////////////////////////////////////////////////////////////////////////////

    function totalSupply() external pure returns (uint256) {
        // Yes, I mean it.
        return 0;
    }

    function balanceOf(address account) override external view returns (uint256) {
        Time t = Time.wrap(uint32(block.timestamp));

        // initial value from universal index
        Value x = uIndexes[account].rtb(t);

        // pending distributions from pool
        if (pools[ISuperTokenPool(account)]) {
            // NB! Please ask solidity designer why "+=" is not derived for overloaded operator custom types
            x = x + ISuperTokenPool(account).getPendingDistribution();
        }

        // pool-connected balance
        EnumerableSet.AddressSet storage connections = _connectionsMap[account];
        for (uint i = 0; i < connections.length(); ++i) {
            address p = connections.at(i);
            x = x + ToySuperTokenPool(p).getClaimable(t, account);
        }

        return Value.unwrap(x) > 0 ? uint256(Value.unwrap(x)) : 0;
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
    // FIXME require(from != to)
    ////////////////////////////////////////////////////////////////////////////////

    function _shift(address from, address to, Value amount, bool checkAllowance) internal
        returns (bool success)
    {
        require(!pools[ISuperTokenPool(to)], "Is a pool!");
        require(Value.unwrap(amount) >= 0, "don't even try");
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

    function flow(address from, address to, FlowId flowId, FlowRate flowRate) override external
        returns (bool success)
    {
        /// check inputs
        // require(FlowRate.unwrap(flowRate) >= 0, "Negative flow rate not allowed.");
        require(!pools[ISuperTokenPool(to)], "Is a pool!");

        /// prepare local variables (let bindings)
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowAddress = keccak256(abi.encode(from, to, flowId));

        // FIXME: plug permission controls
        require(msg.sender == from, "No flow permission");

        // Make updates
        FlowRate flowRateDelta = flowRate - flowRates[flowAddress];
        (uIndexes[from], uIndexes[to]) = uIndexes[from].shiftFlow2a(uIndexes[to], flowRateDelta, t);
        flowRates[flowAddress] = flowRate;
        return true;
    }

    function distribute(address from, ISuperTokenPool to, Value reqAmount) override external
        returns (bool success, Value actualAmount)
    {
        require(pools[to], "Not a pool!");
        // FIXME: plug permission controls
        require(msg.sender == from);
        // Make updates
        PDPoolIndex memory pdidx = to.getIndex();
        (uIndexes[from], pdidx, actualAmount) = uIndexes[from].shift2(pdidx, reqAmount);
        assert(to.operatorSetIndex(pdidx));
        success = true;
    }

    function distributeFlow(address from, ISuperTokenPool to, FlowId flowId, FlowRate reqFlowRate) override external
        returns (bool success, FlowRate actualFlowRate)
    {
        /// check inputs
        // require(FlowRate.unwrap(flowRate) >= 0, "Negative flow rate not allowed.");
        require(pools[to], "Not a pool!");

        /// prepare local variables
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowAddress = keccak256(abi.encode(from, to, flowId)); // TODO maybe flowRef

        // FIXME: plug permission controls
        require(msg.sender == from, "No flow permission");

        // Make updates
        FlowRate oldFlowRate = uIndexes[from].flow_rate.inv();
        PDPoolIndex memory pdidx = to.getIndex();
        (uIndexes[from], pdidx, actualFlowRate) = uIndexes[from].shiftFlow2b
            (pdidx, reqFlowRate - flowRates[flowAddress], t);
        to.operatorSetIndex(pdidx);
        flowRates[flowAddress] = flowRates[flowAddress] + actualFlowRate - oldFlowRate;
        success = true;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    function createPool() external
        returns (ToySuperTokenPool pool)
    {
        pool = new ToySuperTokenPool(msg.sender);
        pools[pool] = true;
        // approve the pool to use its own fund, it is required by ERC20 approval system
        _approve(address(pool), address(pool), type(uint256).max);
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
        returns (bool success) {
        Time t = Time.wrap(uint32(block.timestamp));
        if (doConnect) {
            if (!_connectionsMap[msg.sender].contains(address(to))) {
                _connectionsMap[msg.sender].add(address(to));
                assert(to.operatorConnectMember(t, msg.sender, true));
            }
        } else {
            if (_connectionsMap[msg.sender].contains(address(to))) {
                _connectionsMap[msg.sender].remove(address(to));
                assert(to.operatorConnectMember(t, msg.sender, false));
            }
        }
        return true;
    }

    function isMemberConnected(ISuperTokenPool to, address memberAddr) override external view returns (bool) {
        return _connectionsMap[memberAddr].contains(address(to));
    }

    /// This is used by the pool to adjust flow rate
    function absorbParticleFromPool(address account, BasicParticle calldata p) override external returns (bool) {
        require(pools[ToySuperTokenPool(msg.sender)], "Only absorbing from pools");
        uIndexes[account] = uIndexes[account].mappend(p);
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
