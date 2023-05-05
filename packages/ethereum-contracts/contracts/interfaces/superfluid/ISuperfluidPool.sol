// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import {ISuperfluidToken} from "../superfluid/ISuperfluidToken.sol";

/**
 * @dev The interface for any super token pool regardless of the distribution schemes.
 */
interface ISuperfluidPool {
    // Custom Errors

    error SUPERFLUID_POOL_INVALID_TIME();
    error SUPERFLUID_POOL_NEGATIVE_UNITS_NOT_SUPPORTED();
    error SUPERFLUID_POOL_NOT_POOL_ADMIN();
    error SUPERFLUID_POOL_NOT_GDA();

    // Events
    event MemberUpdated(address indexed member, uint128 units, uint256 updatedAt);
    event PoolIndexUpdated(uint128 totalUnits, uint32 wpSettledAt, int256 wpSettledValue, int96 wpFlowRate);
    event DistributionClaimed(address indexed member, int256 claimableAmount, int256 totalClaimed, uint256 timestamp);

    /// @notice The pool admin
    function admin() external view returns (address);

    /// @notice The SuperToken for the pool
    function superToken() external view returns (ISuperfluidToken);

    /// @notice The total units of the pool
    function getTotalUnits() external view returns (uint128);

    /// @notice The total number of units of disconnected members
    function getDisconnectedUnits() external view returns (uint128);

    /// @notice The total number of units for `memberAddress`
    /// @param memberAddress The address of the member
    function getUnits(address memberAddress) external view returns (uint128);

    /// @notice The flow rate of the connected members
    function getConnectedFlowRate() external view returns (int96);

    /// @notice The flow rate of the disconnected members
    function getDisconnectedFlowRate() external view returns (int96);

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

    /// @notice Does a claim for `memberAddr` at `time` and handles internal accounting logic
    /// @dev This is only callable by the GDA and is not to be used by the end user
    function operatorConnectMember(address memberAddr, bool doConnect, uint32 time) external returns (bool);
}

interface ISuperfluidPoolAdmin {
    /// @notice Returns whether `account` is connected for pool
    function isMemberConnected(ISuperfluidToken token, address pool, address account) external view returns (bool);

    /// @notice Returns the adjustment flow info for `pool`
    function getPoolAdjustmentFlowInfo(ISuperfluidPool pool)
        external
        view
        returns (address recipient, bytes32 flowHash, int96 flowRate);

    /// @notice Handles a settle claim and is called when someone calls claim on the pool
    function poolSettleClaim(ISuperfluidToken superToken, address claimRecipient, int256 amount)
        external
        returns (bool);
}
