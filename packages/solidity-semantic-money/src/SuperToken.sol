// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.18;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";


type FlowId is uint32;

contract Pool {
    // address distributor;
    PDPoolIndex public index;
    mapping (address member => PDPoolMember member_data) public members;

    function updatePoolMember(address pool, address member, Unit unit) external returns (bool) {}
}

contract SuperToken is IERC20 {
    using MonetaryTypes for Time;
    using MonetaryTypes for FlowRate;
    using MonetaryTypes for Value;
    using SemanticMoney for BasicParticle;
    using SemanticMoney for PDPoolMemberMU;

    // immutable internal Pool _POOL_IMPLEMENTATION;
    //FlowId constant public ALL_FLOWS_FLOW_ID = FlowId.wrap(type(uint32).max);

    mapping (address owner => BasicParticle) public uIndexes;
    mapping (bytes32 flowAddress => FlowRate) public flowRates;

    ////////////////////////////////////////////////////////////////////////////////
    // ERC20 operations
    ////////////////////////////////////////////////////////////////////////////////

    function allowance(address owner, address spender) external view returns (uint256) {}

    function approve(address spender, uint256 amount) external returns (bool) {}

    function balanceOf(address account) external view returns (uint256) {
        Time t = Time.wrap(uint32(block.timestamp));
        int256 x = Value.unwrap(uIndexes[account].rtb(t));
        return x > 0 ? uint256(x) : 0;
    }

    function totalSupply() external view returns (uint256) {}

    function transfer(address to, uint256 amount) external returns (bool) {
        return transferFrom(msg.sender, to, amount);
    }

    function transferFrom(address from, address to, uint256 amount) public returns (bool) {
        Time t = Time.wrap(uint32(block.timestamp));
        // FIXME: plug permission controls
        require(msg.sender == from);
        // Make updates
        (uIndexes[from], uIndexes[to]) = uIndexes[from].shift2(uIndexes[to], Value.wrap(int256(amount)), t);
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives
    ////////////////////////////////////////////////////////////////////////////////

    function iTransfer(address from, address to, Value amount) external
        returns (bool success) {}

    function distribute(address from, Pool to, Value reqAmount) external
        returns (bool success, Value actualAmount) {
    }

    function flow(address from, address to, FlowId flowId, FlowRate flowRate) external
        returns (bool success) {
        Time t = Time.wrap(uint32(block.timestamp));
        bytes32 flowAddress = keccak256(abi.encode(from, to, flowId));
        // FIXME: plug permission controls
        require(msg.sender == from);
        // Make updates
        FlowRate oldFlowRate = flowRates[flowAddress];
        (uIndexes[from], uIndexes[to]) = uIndexes[from].flow2(uIndexes[to], flowRate.sub(oldFlowRate), t);
        flowRates[flowAddress] = oldFlowRate;
        return false;
    }

    function distributeFlow(address from, Pool to, FlowId flowId, FlowRate reqFlowRate) external
        returns (bool success, uint256 actualFlowRate) {}

    // Desirable option: isPool(to) ? without lookup table, O(1) !! NOT POSSIBLE !!
    // Other options:
    //   a) use flowId
    //   b) split into two functions: flow, distributeFlow

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    function createPool() external returns (Pool pool) {
        return new Pool();
    }

}
