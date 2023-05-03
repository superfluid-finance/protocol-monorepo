// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";

/**
 * @dev The interface for any super token pool regardless of the distribution schemes.
 */
interface ISuperfluidPool {
    // Structs
    struct PoolIndexData {
        uint128 totalUnits;
        uint32 wrappedSettledAt;
        int96 wrappedFlowRate;
        int256 wrappedSettledValue;
    }

    struct MemberData {
        uint128 ownedUnits;
        uint32 syncedSettledAt;
        int96 syncedFlowRate;
        int256 syncedSettledValue;
        int256 settledValue;
        int256 claimedValue;
    }

    // Custom Errors
    error SUPERFLUID_POOL_INVALID_TIME();
    error SUPERFLUID_POOL_NEGATIVE_UNITS_NOT_SUPPORTED();
    error SUPERFLUID_POOL_NOT_POOL_ADMIN();
    error SUPERFLUID_POOL_NOT_GDA();

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

    function getIndex() external view returns (PoolIndexData memory);

    function getTotalUnits() external view returns (uint128);

    function getDisconnectedUnits() external view returns (uint128);

    function getUnits(address memberAddress) external view returns (uint128);

    function getConnectedFlowRate() external view returns (int96);

    function getDisconnectedFlowRate() external view returns (int96);

    function getDisconnectedBalance(
        uint32 time
    ) external view returns (int256 balance);

    function getMemberFlowRate(
        address memberAddress
    ) external view returns (int96);

    function getClaimable(
        address memberAddr,
        uint32 time
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
        address memberAddr,
        bool doConnect,
        uint32 time
    ) external returns (bool);
}

interface ISuperfluidPoolAdmin {
    function isMemberConnected(
        ISuperfluidToken token,
        address pool,
        address member
    ) external view returns (bool);

    function getPoolAdjustmentFlowInfo(
        ISuperfluidPool pool
    )
        external
        view
        returns (address recipient, bytes32 flowHash, int96 flowRate);

    function poolSettleClaim(
        ISuperfluidToken superToken,
        address claimRecipient,
        int256 amount
    ) external returns (bool);
}
