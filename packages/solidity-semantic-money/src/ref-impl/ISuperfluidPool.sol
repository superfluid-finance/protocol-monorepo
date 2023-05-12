// SPDX-License-Identifier: UNLICENSED
pragma solidity >= 0.8.4;

import {
    Time, Value, FlowRate, Unit,
    BasicParticle, PDPoolIndex
} from "../SemanticMoney.sol";


/**
 * @dev The interface for any super token pool regardless of the distribution schemes.
 */
interface ISuperfluidPool {
    function getIndex() external view returns (PDPoolIndex memory);

    function getTotalUnits() external view returns (Unit);

    function getDisconnectedUnits() external view returns (Unit);

    function getUnits(address memberAddress) external view returns (Unit);

    function getDistributionFlowRate() external view returns (FlowRate);

    function getConnectedFlowRate() external view returns (FlowRate);

    function getDisconnectedFlowRate() external view returns (FlowRate);

    /**
     * @dev Disconnected balance, or all disconnected members' claimable balances.
     *
     * Property:
     *
     *   (a) pool.getDisonnectedBalance(t) == foldr sum (\m -> m.getClaimable(m, t)) 0 (isDisconnectd pool.members)
     */
    function getDisconnectedBalance(Time t) external view returns (Value);

    function getMemberFlowRate(address memberAddress) external view returns (FlowRate);

    function getClaimable(address memberAddr) external view returns (Value);

    function getClaimable(address memberAddr, Time t) external view returns (Value);

    function updateMember(address memberAddr, Unit unit) external returns (bool);

    function claimAll(address memberAddr) external returns (bool);

    function claimAll() external returns (bool);

    function operatorSetIndex(PDPoolIndex calldata index) external returns (bool);

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    function operatorConnectMember(address memberAddr, bool doConnect, Time t) external returns (bool);
}

/**
 * @dev The interface for the operator of a super token pool
 */
interface ISuperfluidPoolOperator {
    /// Check if an address is connected to the pool
    function isMemberConnected(ISuperfluidPool pool, address memberAddr) external view returns (bool);

    /// Get pool adjustment flow information: (recipient, flowHahs, flowRate)
    function getPoolAdjustmentFlowInfo(ISuperfluidPool pool) external view returns (address, bytes32, FlowRate);

    /// Update the adjustment flow rate
    function appendIndexUpdateByPool(BasicParticle memory p, Time t) external returns (bool);

    /// Settle the claim
    function poolSettleClaim(address claimRecipient, Value amount) external returns (bool);
}