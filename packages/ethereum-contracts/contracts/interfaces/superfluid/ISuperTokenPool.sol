// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";

/**
 * @dev The interface for any super token pool regardless of the distribution schemes.
 */
interface ISuperTokenPool {
    // Custom Errors
    error SUPER_TOKEN_POOL_NEGATIVE_UNITS_NOT_SUPPORTED();
    error SUPER_TOKEN_POOL_NOT_POOL_ADMIN();

    // Events
    event PoolIndexUpdated(
        ISuperfluidToken indexed token,
        int128 totalUnits,
        uint32 wpSettledAt,
        int256 wpSettledValue,
        int96 wpFlowRate
    );

    function _superToken() external view returns (ISuperfluidToken);

    function getTotalUnits() external view returns (int128);

    function getUnits(address memberAddress) external view returns (int128);

    function getDistributionFlowRate() external view returns (int96);

    function getPendingDistributionFlowRate() external view returns (int96);

    function getMemberFlowRate(address memberAddress) external view returns (int96);

    function getPendingDistribution() external view returns (int256);

    function getClaimable(uint32 time, address memberAddr) external view returns (int256);

    function getClaimableNow(address memberAddr) external view returns (int256 claimableBalance, uint256 timestamp);

    function updateMember(address memberAddr, int128 unit) external returns (bool);    

    function claimAll(uint32 time, address memberAddr) external returns (bool);

    function claimAll(address memberAddr) external returns (bool);

    function claimAll() external returns (bool);

    function operatorConnectMember(uint32 time, address memberAddr, bool doConnect) external returns (bool);
}
