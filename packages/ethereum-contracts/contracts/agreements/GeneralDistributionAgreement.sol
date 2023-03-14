// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperApp,
    SuperAppDefinitions,
    ContextDefinitions,
    SuperfluidGovernanceConfigs
} from "../interfaces/superfluid/ISuperfluid.sol";
import {
    BasicParticle,
    SemanticMoney,
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
    FlowRate,
    Time,
    Unit,
    Value
} from "../libs/SemanticMoney.sol";
import { SuperTokenPool } from "../superfluid/SuperTokenPool.sol";
import {
    IGeneralDistributionAgreementV1,
    ISuperfluidToken
} from "../interfaces/agreements/IGeneralDistributionAgreementV1.sol";
import { ISuperTokenPool } from "../interfaces/superfluid/ISuperTokenPool.sol";
import { SlotsBitmapLibrary } from "../libs/SlotsBitmapLibrary.sol";
import { AgreementBase } from "./AgreementBase.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";

/**
 * @title General Distribution Agreement
 * @author Superfluid
 * @notice
 */
contract GeneralDistributionAgreementV1 is
    AgreementBase,
    IGeneralDistributionAgreementV1
{
    address public constant SLOTS_BITMAP_LIBRARY_ADDRESS = address(SlotsBitmapLibrary);
    /// @dev Pool Member state slot id for storing membership bitmap
    uint256 private constant _POOL_MEMBERSHIPS_BITMAP_STATE_SLOT_ID = 0;
    /// @dev Pool Member state slot id starting point for PoolMember state
    uint256 private constant _POOL_MEMBERSHIP_DATA_STATE_SLOT_ID_START = 1 << 128;
    constructor(ISuperfluid host) AgreementBase(address(host)) {}

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        ISuperfluidToken token,
        address account,
        uint256 time
    )
        external
        view
        override
        returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
    {
        uint32[] memory slotIds;
        bytes32[] memory midList;
        (slotIds, midList) = _listMembershipIds(token, account);

        for (uint i = 0; i < midList.length; i++) {
            bool exist;
            PDPoolMember memory memberData;
            bytes32 poolIndexId;

            {
                uint32 membershipSlotId = slotIds[i];
                (exist, memberData) = _getPoolMemberData(token, midList[i]);
                assert(exist);
                poolIndexId = token.getAgreementStateSlot(
                    address(this),
                    account,
                    _POOL_MEMBERSHIP_DATA_STATE_SLOT_ID_START + membershipSlotId, 1)[0];
            }

            {
                address pool = _getPoolAddressfromPoolIndexId(poolIndexId);
                assert(exist);

                dynamicBalance =
                    dynamicBalance +
                    ISuperTokenPool(pool).getClaimable(uint32(time), account);
            }
        }
    }

    function createPool(
        ISuperfluidToken token
    ) external returns (SuperTokenPool pool) {
        pool = new SuperTokenPool(msg.sender);
    }

    // @note This is same as approveSubscription
    function connectPool(SuperTokenPool pool) external returns (bool success) {
        // connect via slotsbitmap
    }

    // @note This is same as revokeSubscription
    function disconnectPool(SuperTokenPool pool) external returns (bool success) {
        // disconnect via slotsbitmap
    }

    function distribute(
        ISuperfluidToken token,
        ISuperTokenPool pool,
        uint256 amount,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);
        newCtx = ctx;
    }

    function distributeFlow(
        ISuperfluidToken token,
        ISuperTokenPool pool,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);

        newCtx = ctx;
    }

    // # Pool Index AgreementData operations
    //
    // AgreementData packing:
    //
    // -------- --------------- --------------- ---------------
    // WORD 1: |   totalUnits  |   settledAt   |    flowRate   |
    // -------- --------------- --------------- ---------------
    //         |      128b     |      32b      |      96b      |
    // -------- --------------- --------------- ---------------
    // WORD 2: |              settledValue (/unit)             |
    // -------- --------------- --------------- ---------------
    //         |                      256b                     |
    // -------- --------------- --------------- ---------------

    function _getPoolIndexId(address pool) private pure returns (bytes32) {
        return bytes32(uint256(uint160(pool)));
    }

    function _getPoolAddressfromPoolIndexId(
        bytes32 poolIndexId
    ) private pure returns (address) {
        return address(uint160(uint256(poolIndexId)));
    }

    function _encodePoolIndexData(
        PDPoolIndex memory index
    ) private pure returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] = bytes32(
            (uint256(int256(Unit.unwrap(index.total_units))) << 128)              |
            (uint256(Time.unwrap(index.wrapped_particle.settled_at)) << 96)       |
            (uint256(uint128(FlowRate.unwrap(index.wrapped_particle.flow_rate))))
        );
        // @note this is per unit settled value
        data[1] = bytes32(uint256(Value.unwrap(index.wrapped_particle.settled_value)));
    }

    function _getPoolIndexData(
        ISuperfluidToken token,
        bytes32 poolId
    ) private view returns (bool exists, PDPoolIndex memory index) {
        bytes32[] memory data = token.getAgreementData(address(this), poolId, 3);

        uint256 wordA = uint256(data[0]);
        uint256 wordB = uint256(data[1]);

        exists = wordA > 0;

        if (exists) {
            index = PDPoolIndex({
                total_units: Unit.wrap(int128(int256(wordA >> 128))),
                wrapped_particle: BasicParticle({
                    settled_at: Time.wrap(uint32(wordA >> 96)),
                    flow_rate: FlowRate.wrap(int96(int256(wordA))),
                    settled_value: Value.wrap(int256(wordB))
                })
            });
        }
    }

    // # Pool Member AgreementData operations
    //
    // AgreementData packing:
    //
    // -------- ------------------ ------------------ ------------------
    // WORD 1: |    ownedUnits    |     settledAt    | flowRate (/unit) |
    // -------- ------------------ ------------------ ------------------
    //         |       128b       |       32b        |        96b       |
    // -------- ------------------ ------------------ ------------------
    // WORD 2: |                  settledValue (/unit)                  |
    // -------- ------------------ ------------------ ------------------
    //         |                          256b                          | 
    // -------- ------------------ ------------------ ------------------
    // WORD 3: |                      settledValue                      |
    // -------- ------------------ ------------------ ------------------
    //         |                          256b                          | 
    // -------- ------------------ ------------------ ------------------

    function _getPoolMemberId(
        address subscriber,
        bytes32 poolIndexId
    ) private pure returns (bytes32) {
        return keccak256(abi.encodePacked("poolMember", subscriber, poolIndexId));
    }

    function _encodePoolMemberData(
        PDPoolMember memory member
    ) private pure returns (bytes32[] memory data) {
        data = new bytes32[](3);
        data[0] = bytes32(
            (uint256(int256(Unit.unwrap(member.owned_units))) << 128)             |
            (uint256(Time.unwrap(member.synced_particle.settled_at)) << 96)       |
            (uint256(uint128(FlowRate.unwrap(member.synced_particle.flow_rate))))
        );
        data[1] = bytes32(uint256(Value.unwrap(member.synced_particle.settled_value)));
        data[2] = bytes32(uint256(Value.unwrap(member.settled_value)));
    }

    function _getPoolMemberData(
        ISuperfluidToken token,
        bytes32 poolMemberId
    ) private view returns (bool exists, PDPoolMember memory member) {
        bytes32[] memory data = token.getAgreementData(address(this), poolMemberId, 2);

        uint256 wordA = uint256(data[0]);
        uint256 wordB = uint256(data[1]);

        exists = wordA > 0;

        if (exists) {
            member = PDPoolMember({
                owned_units: Unit.wrap(int128(int256(wordA >> 128))),
                synced_particle: BasicParticle({
                    settled_at: Time.wrap(uint32(wordA >> 96)),
                    flow_rate: FlowRate.wrap(int96(int256(wordA))),
                    settled_value: Value.wrap(int256(wordB))
                }),
                settled_value: Value.wrap(int256(wordB))
            });
        }
    }

    function _findAndFillMembershipsBitmap(
        ISuperfluidToken token,
        address poolMember,
        bytes32 poolIndexId
    ) private returns (uint32 slotId) {
        return
            SlotsBitmapLibrary.findEmptySlotAndFill(
                token,
                poolMember,
                _POOL_MEMBERSHIPS_BITMAP_STATE_SLOT_ID,
                _POOL_MEMBERSHIP_DATA_STATE_SLOT_ID_START,
                poolIndexId
            );
    }

    function _clearMembershipsBitmap(
        ISuperfluidToken token,
        address poolMember,
        uint32 slotId
    ) private {
        SlotsBitmapLibrary.clearSlot(
            token,
            poolMember,
            _POOL_MEMBERSHIPS_BITMAP_STATE_SLOT_ID,
            slotId
        );
    }

    function _listMembershipIds(
        ISuperfluidToken token,
        address poolMember
    ) private view returns (uint32[] memory slotIds, bytes32[] memory midList) {
        // @note midList here is actually a list of poolIndexId
        (slotIds, midList) = SlotsBitmapLibrary.listData(
            token,
            poolMember,
            _POOL_MEMBERSHIPS_BITMAP_STATE_SLOT_ID,
            _POOL_MEMBERSHIP_DATA_STATE_SLOT_ID_START
        );

        for (uint i = 0; i < midList.length; ++i) {
            midList[i] = _getPoolMemberId(poolMember, midList[i]);
        }
    }
}
