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

    function getUnits(address memberAddress) external view returns (Unit);

    function getDistributionFlowRate() external view returns (FlowRate);

    function getPendingDistributionFlowRate() external view returns (FlowRate);

    function getMemberFlowRate(address memberAddress) external view returns (FlowRate);

    function getPendingDistribution() external view returns (Value);

    function getClaimable(Time t, address memberAddr) external view returns (Value);

    function getClaimable(address memberAddr) external view returns (Value);

    function updateMember(address memberAddr, Unit unit) external returns (bool);

    function claimAll(address memberAddr) external returns (bool);

    function claimAll() external returns (bool);

    function operatorSetIndex(PDPoolIndex calldata index) external returns (bool);

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    function operatorConnectMember(Time t, address memberAddr, bool doConnect) external returns (bool);
}

/**
 * @dev The interface for the admin of a super token pool admin
 */
interface ISuperfluidPoolAdmin {
    /// Check if an address is connected to the pool
    function isMemberConnected(ISuperfluidPool pool, address memberAddr) external view
        returns (bool);

    /// This is used by the pool to adjust flow rate
    function absorbParticlesFromPool(address[] calldata accounts, BasicParticle[] calldata ps) external
        returns (bool);
}
