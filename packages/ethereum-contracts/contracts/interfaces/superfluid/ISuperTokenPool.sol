// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";

/**
 * @dev The interface for any super token pool regardless of the distribution schemes.
 */
interface ISuperTokenPool {
    // Custom Errors
    error SUPER_TOKEN_POOL_INVALID_TIME();                  // 0xd469ac52
    error SUPER_TOKEN_POOL_NEGATIVE_UNITS_NOT_SUPPORTED();  // 0xd568f5c5
    error SUPER_TOKEN_POOL_NOT_POOL_ADMIN();                // 0xe448e00d
    error SUPER_TOKEN_POOL_NOT_GDA();                       // 0xb3a64080

    // Events
    event MemberUpdated(
        address indexed member,
        uint128 units,
        uint256 updatedAt
    );
    event PoolIndexUpdated(
        uint128 totalUnits,
        uint32 wpSettledAt,
        int256 wpSettledValue,
        int96 wpFlowRate
    );
    event DistributionClaimed(
        address indexed member,
        int256 claimableAmount,
        int256 totalClaimed,
        uint256 timestamp
    );

    function admin() external view returns (address);

    function superToken() external view returns (ISuperfluidToken);

    function pendingUnits() external view returns (uint128);

    function getTotalUnits() external view returns (uint128);

    function getUnits(address memberAddress) external view returns (uint128);

    function getDistributionFlowRate() external view returns (int96);

    function getPendingDistributionFlowRate() external view returns (int96);

    function getMemberFlowRate(
        address memberAddress
    ) external view returns (int96);

    function getPendingDistribution() external view returns (int256);

    function getClaimable(
        uint32 time,
        address memberAddr
    ) external view returns (int256);

    function getClaimableNow(
        address memberAddr
    ) external view returns (int256 claimableBalance, uint256 timestamp);

    function updateMember(
        address memberAddr,
        uint128 unit
    ) external returns (bool);

    function claimAll(address memberAddr) external returns (bool);

    function claimAll() external returns (bool);

    function operatorConnectMember(
        uint32 time,
        address memberAddr,
        bool doConnect
    ) external returns (bool);
}
