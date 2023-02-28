// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import { EnumerableMap } from "@openzeppelin/contracts/utils/structs/EnumerableMap.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";


type FlowId is uint32;

/**
 * @dev A proportional distribution pool.
 */
contract Pool is Ownable {
    using SemanticMoney for PDPoolMemberMU;

    address public distributor;
    PDPoolIndex internal _index;
    mapping (address member => PDPoolMember member_data) internal _members;

    constructor (address distributor_) Ownable() {
        distributor = distributor_;
    }

    // NOTE: Solidity public function for the storage fields do not support structs.
    //       They are added manually instead.

    function getIndex() external view returns (PDPoolIndex memory) { return _index; }
    function setIndex(PDPoolIndex calldata index) onlyOwner external { _index = index; }
    function getMember(address member) external view returns (PDPoolMember memory) { return _members[member]; }

    function updatePoolMember(address member, Unit unit) external returns (bool) {
        require(Unit.unwrap(unit) >= 0, "Negative unit number not supported");
        require(msg.sender == distributor, "not the distributor!");
        Time t = Time.wrap(uint32(block.timestamp));
        BasicParticle memory p;
        (_index, _members[member], p) = PDPoolMemberMU(_index, _members[member]).pool_member_update(p, unit, t);
        SuperToken(owner()).absorb(distributor, p);
        return true;
    }

    // claim()
}

/**
 * @dev A very special super token for testing and fun.
 *
 * Features:
 * - Pure super token, no ERC20 wrapping business.
 * - Negative account is allowed,
 * - no permission control for account going negative.
 */
contract SuperToken is IERC20 {
    using MonetaryTypes for Time;
    using MonetaryTypes for FlowRate;
    using MonetaryTypes for Value;
    using SemanticMoney for BasicParticle;
    using SemanticMoney for PDPoolMemberMU;
    using EnumerableMap for EnumerableMap.AddressToUintMap;

    // immutable internal Pool _POOL_IMPLEMENTATION;
    //FlowId constant public ALL_FLOWS_FLOW_ID = FlowId.wrap(type(uint32).max);

    mapping (address owner => BasicParticle) public uIndexes;
    mapping (bytes32 flowAddress => FlowRate) public flowRates;
    mapping (Pool pool => bool exist) public pools;
    mapping (address owner => EnumerableMap.AddressToUintMap) internal _connectionsMap;
    mapping(address => mapping(address => uint256)) private _allowances;

    ////////////////////////////////////////////////////////////////////////////////
    // ERC20 operations
    ////////////////////////////////////////////////////////////////////////////////

    function totalSupply() external pure returns (uint256) {
        // Yes, I mean it.
        return 0;
    }

    function balanceOf(address account) external view returns (uint256) {
        Time t = Time.wrap(uint32(block.timestamp));
        int256 x = Value.unwrap(uIndexes[account].rtb(t));
        EnumerableMap.AddressToUintMap storage connections = _connectionsMap[account];
        for (uint i = 0; i < connections.length(); ++i) {
            (address p,) = connections.at(i);
            x += Value.unwrap(PDPoolMemberMU(Pool(p).getIndex(), Pool(p).getMember(account)).rtb(t));
        }
        return x > 0 ? uint256(x) : 0;
    }

    function transfer(address to, uint256 amount) external returns (bool) {
        address owner = msg.sender;
        _transfer(owner, to, amount);
        return true;
    }

    function transferFrom(address from, address to, uint256 amount) external returns (bool) {
        return _transferFrom(from, to, amount);
    }

    function _transferFrom(address from, address to, uint256 amount) internal returns (bool) {
        address spender = msg.sender;
        _spendAllowance(from, spender, amount);
        _transfer(from, to, amount);
        return true;
    }

    function _transfer(address from, address to, uint256 amount) internal {
        // Make updates
        (uIndexes[from], uIndexes[to]) = uIndexes[from].shift2(uIndexes[to], Value.wrap(int256(amount)));
    }

    function allowance(address owner, address spender) external view returns (uint256) {
        return _allowances[owner][spender];
    }

    function approve(address spender, uint256 amount) external returns (bool) {
        address owner = msg.sender;
        _approve(owner, spender, amount);
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives
    ////////////////////////////////////////////////////////////////////////////////

    // This is the non-ERC20 version of instant transfer, that can trigger actions defined by "to"
    function iTransfer(address from, address to, Value amount) external
        returns (bool success) {
        require(Value.unwrap(amount) >= 0, "don't even try");
        return _transferFrom(from, to, uint256(Value.unwrap(amount)));
    }

    // flowRef??? = keccack256(abi.encode(block.chainId, from, to, subFlowId));

    function flow(address from, address to, FlowId flowId, FlowRate flowRate) external
        returns (bool success) {
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowAddress = keccak256(abi.encode(from, to, flowId));
        // FIXME: plug permission controls
        require(msg.sender == from);
        // Make updates
        (uIndexes[from], uIndexes[to]) = uIndexes[from].flow2(uIndexes[to], flowRate, t);
        flowRates[flowAddress] = flowRate;
        return true;
    }

    function distribute(address from, Pool to, Value reqAmount) external
        returns (bool success, Value actualAmount) {
        require(pools[to], "Not a pool!");
        // FIXME: plug permission controls
        require(msg.sender == from);
        require(to.distributor() == from, "Not the distributor!");
        // Make updates
        PDPoolIndex memory pdidx = to.getIndex();
        (uIndexes[from], pdidx, actualAmount) = uIndexes[from].shift2(pdidx, reqAmount);
        to.setIndex(pdidx);
        success = true;
    }

    function distributeFlow(address from, Pool to, FlowId flowId, FlowRate reqFlowRate) external
        returns (bool success, FlowRate actualFlowRate) {
        require(pools[to], "Not a pool!");
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowAddress = keccak256(abi.encode(from, to, flowId));
        // FIXME: plug permission controls
        require(msg.sender == from);
        require(to.distributor() == from, "Not the distributor!");
        // Make updates
        PDPoolIndex memory pdidx = to.getIndex();
        (uIndexes[from], pdidx, actualFlowRate) = uIndexes[from].flow2(pdidx, reqFlowRate, t);
        to.setIndex(pdidx);
        flowRates[flowAddress] = actualFlowRate;
        success = true;
    }

    function connectPool(Pool to) external
        returns (bool success) {
        return connectPool(to, true);
    }

    function disconnectPool(Pool to) external
        returns (bool success) {
        return connectPool(to, false);
    }

    function connectPool(Pool to, bool doConnect) public
        returns (bool success) {
        if (doConnect) {
            _connectionsMap[msg.sender].set(address(to), 0);
        } else {
            _connectionsMap[msg.sender].remove(address(to));
        }
        return true;
    }

    // Desirable option: isPool(to) ? without lookup table, O(1) !! NOT POSSIBLE !!
    // Other options:
    //   a) use flowId
    //   b) split into two functions: flow, distributeFlow

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    function createPool() external returns (Pool pool) {
        pool = new Pool(msg.sender);
        pools[pool] = true;
    }

    function absorb(address account, BasicParticle calldata p) external {
        require(pools[Pool(msg.sender)], "Only absorbing from pools");
        uIndexes[account] = uIndexes[account].mappend(p);
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
