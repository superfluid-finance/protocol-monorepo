// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

// Unit = int128
// Value = int256
// FlowRate = int96

/**
 * @dev The interface for any super token pool regardless of the distribution schemes.
 */
interface ISuperTokenPool {
    function getPendingDistribution() external view returns (int256);

    function getClaimable(
        uint32 time,
        address memberAddr
    ) external view returns (int256);

    function claimAll(uint32 time, address memberAddr) external returns (bool);
}
