// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";

/**
 * @dev The interface for any super token pool regardless of the distribution schemes.
 */
interface ISuperfluidPool {
    // Custom Errors

    error SUPERFLUID_POOL_INVALID_TIME();       // 0x83c35016
    error SUPERFLUID_POOL_NO_POOL_MEMBERS();    // 0xe10f405a
    error SUPERFLUID_POOL_NO_ZERO_ADDRESS();
    error SUPERFLUID_POOL_NOT_POOL_ADMIN();     // 0x7b0be922
    error SUPERFLUID_POOL_NOT_GDA();            // 0xfcbe3f9e

    // Events
    event MemberUpdated(address indexed member, uint128 units);
    event DistributionClaimed(address indexed member, int256 claimableAmount, int256 totalClaimed);

    /// @notice The pool admin
    /// @dev The admin is the creator of the pool and has permissions to update member units
    /// and is the recipient of the adjustment flow rate
    function admin() external view returns (address);

    /// @notice The SuperToken for the pool
    function superToken() external view returns (ISuperfluidToken);

    /// @notice The total units of the pool
    function getTotalUnits() external view returns (uint128);

    /// @notice The total number of units of connected members
    function getTotalConnectedUnits() external view returns (uint128);

    /// @notice The total number of units of disconnected members
    function getTotalDisconnectedUnits() external view returns (uint128);

    /// @notice The total number of units for `memberAddress`
    /// @param memberAddress The address of the member
    function getUnits(address memberAddress) external view returns (uint128);

    /// @notice The flow rate of the connected members
    function getTotalConnectedFlowRate() external view returns (int96);

    /// @notice The flow rate of the disconnected members
    function getTotalDisconnectedFlowRate() external view returns (int96);

    /// @notice The balance of all the disconnected members at `time`
    /// @param time The time to query
    function getDisconnectedBalance(uint32 time) external view returns (int256 balance);

    /// @notice The flow rate a member is receiving from the pool
    /// @param memberAddress The address of the member
    function getMemberFlowRate(address memberAddress) external view returns (int96);

    /// @notice The claimable balance for `memberAddr` at `time` in the pool
    /// @param memberAddr The address of the member
    /// @param time The time to query
    function getClaimable(address memberAddr, uint32 time) external view returns (int256);

    /// @notice The claimable balance for `memberAddr` at `block.timestamp` in the pool
    /// @param memberAddr The address of the member
    function getClaimableNow(address memberAddr) external view returns (int256 claimableBalance, uint256 timestamp);

    /// @notice Sets `memberAddr`'s ownedUnits to `newUnits`
    /// @param memberAddr The address of the member
    /// @param newUnits The new units for the member
    function updateMember(address memberAddr, uint128 newUnits) external returns (bool);

    /// @notice Claims the claimable balance for `memberAddr` at `block.timestamp`
    /// @param memberAddr The address of the member
    function claimAll(address memberAddr) external returns (bool);

    /// @notice Claims the claimable balance for `msg.sender` at `block.timestamp`
    function claimAll() external returns (bool);
}
