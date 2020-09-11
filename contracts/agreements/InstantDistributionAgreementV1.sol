// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { IInstantDistributionAgreementV1, ISuperToken } from "../interfaces/IInstantDistributionAgreementV1.sol";
import { ISuperfluid } from "../interfaces/ISuperfluid.sol";
import { ContextLibrary } from "../superfluid/ContextLibrary.sol";

contract InstantDistributionAgreementV1 is IInstantDistributionAgreementV1 {

    uint32 public constant MAX_NUM_SLOTS = 256;

    uint256 public constant SUBSCRIPTION_STATE_SLOT_BITMAP_ID = 0;
    uint256 public constant SUBSCRIPTION_STATE_SLOT_DATA_ID_START = 1 << 128;

    struct PublisherData {
        uint128 indexValue;
        uint128 totalUnitsApproved;
        uint128 totalUnitsPending;
    }

    struct SubscriptionData {
        address publisher;
        uint32 indexId;
        uint32 slotId;
        uint128 indexValue;
        uint128 units;
    }

    function agreementType() external override pure returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");
    }

    function realtimeBalanceOf(
        ISuperToken token,
        address subscriber,
        bytes calldata /* state */,
        uint256 /*time*/
    )
        external
        view
        override
        returns (int256 amount) {
        bool exist;
        PublisherData memory pdata;
        SubscriptionData memory sdata;
        uint256 slotBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            subscriber,
            SUBSCRIPTION_STATE_SLOT_BITMAP_ID, 1)[0]);
        // read all slots
        for (uint32 slotId = 0; slotId < MAX_NUM_SLOTS; ++slotId) {
            if ((uint256(slotBitmap >> slotId) & 1) == 0) continue;
            bytes32 pId = token.getAgreementStateSlot(
                address(this),
                subscriber,
                SUBSCRIPTION_STATE_SLOT_DATA_ID_START + slotId, 1)[0];
            bytes32 sId = _getSubscriptionId(subscriber, pId);
            (exist, pdata) = _getPublisherData(token, pId);
            require(exist, "IDAv1: index does not exist");
            (exist, sdata) = _getSubscriptionData(token, sId);
            require(exist, "IDAv1: subscription does not exist");
            require(sdata.slotId == slotId, "IDAv1: incorrect slot id");
            amount += int256(pdata.indexValue - sdata.indexValue) * int256(sdata.units);
        }
    }

    function getDeposit(
        bytes calldata /*data*/
    )
        external
        pure
        override
        returns(int256 deposit, int256 ownedDeposit)
    {
        return (0, 0);
    }

    function touch(
        address /*account*/,
        bytes calldata state,
        uint256 /*time*/
    )
        external
        pure
        override
        returns(bytes memory newState) {
        // TODO, touch it baby
        newState = state;
    }

    function createIndex(
        ISuperToken token,
        uint32 indexId,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        //ISuperfluid host = ISuperfluid(msg.sender);
        address publisher = ContextLibrary.decode(ctx).msgSender;
        bytes32 pId = _getPublisherId(publisher, indexId);
        require(!_hasPublisherData(token, pId), "IDAv1: index already exists");
        token.createAgreement2(pId, _encodePublisherData(PublisherData(0, 0, 0)));
        // TODO
        newCtx = ctx;
    }

    function updateIndex(
        ISuperToken token,
        uint32 indexId,
        uint128 indexValue,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        address publisher = ContextLibrary.decode(ctx).msgSender;
        bytes32 pId = _getPublisherId(publisher, indexId);
        (bool exist, PublisherData memory pdata) = _getPublisherData(token, pId);
        require(exist, "IDAv1: index does not exist");
        require(indexValue >= pdata.indexValue, "IDAv1: index value should grow");
        int256 deduction = - int256(indexValue - pdata.indexValue) * int256(pdata.totalUnitsApproved);
        pdata.indexValue = indexValue;
        token.updateAgreementData2(pId, _encodePublisherData(pdata));
        token.settleBalance(publisher, deduction);
        // TODO
        newCtx = ctx;
    }

    function getIndex(
        ISuperToken token,
        address publisher,
        uint32 indexId)
            external
            view
            override
            returns(uint128 indexValue, uint128 totalUnits) {
        bytes32 pId = _getPublisherId(publisher, indexId);
        (bool exist, PublisherData memory pdata) = _getPublisherData(token, pId);
        require(exist, "IDAv1: index does not exist");
        indexValue = pdata.indexValue;
        totalUnits = pdata.totalUnitsApproved;
    }

    function approveSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        address subscriber = ContextLibrary.decode(ctx).msgSender;
        bytes32 pId = _getPublisherId(publisher, indexId);
        (bool exist, PublisherData memory pdata) = _getPublisherData(token, pId);
        require(exist, "IDAv1: index does not exist");
        uint32 slotId = _findAndFillSlot(token, subscriber, pId);
        bytes32 sId = _getSubscriptionId(subscriber, pId);
        require(!_hasSubscriptionData(token, sId), "IDAv1: subscription already exists");
        // add to subscription list of the subscriber
        SubscriptionData memory sdata = SubscriptionData({
            publisher: publisher,
            indexId: indexId,
            slotId: slotId,
            units: 0,
            indexValue: pdata.indexValue
        });
        token.createAgreement2(sId, _encodeSubscriptionData(sdata));
        // TODO
        newCtx = ctx;
    }

    function updateSubscription(
        ISuperToken token,
        uint32 indexId,
        address subscriber,
        uint128 units,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        bool exist;
        SubscriptionData memory sdata;
        PublisherData memory pdata;
        address publisher = ContextLibrary.decode(ctx).msgSender;
        bytes32 pId = _getPublisherId(publisher, indexId);
        bytes32 sId = _getSubscriptionId(subscriber, pId);
        (exist, pdata) = _getPublisherData(token, pId);
        require(exist, "IDAv1: index does not exist");
        (exist, sdata) = _getSubscriptionData(token, sId);
        require(sdata.publisher == publisher, "IDAv1: incorrect publisher");
        require(sdata.indexId == indexId, "IDAv1: incorrect indexId");
        require(exist, "IDAv1: subscription does not exist");
        // update total units
        if (units > sdata.units) {
            pdata.totalUnitsApproved += units - sdata.units; // FIXME safe128
        } else {
            pdata.totalUnitsApproved -= units - sdata.units; // FIXME safe128
        }
        token.updateAgreementData2(pId, _encodePublisherData(pdata));
        // update subscriptiond data
        // settle static balance delta before changing the state
        token.settleBalance(
            subscriber,
            int256(pdata.indexValue - sdata.indexValue) * int256(sdata.units)
        );
        sdata.indexValue = pdata.indexValue;
        sdata.units = units;
        token.updateAgreementData2(sId, _encodeSubscriptionData(sdata));
        // TODO
        newCtx = ctx;
    }

    function getSubscriptionUnits(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber)
            external
            view
            override
            returns(uint128 units) {
        bool exist;
        PublisherData memory pdata;
        SubscriptionData memory sdata;
        bytes32 pId = _getPublisherId(publisher, indexId);
        (exist, pdata) = _getPublisherData(token, pId);
        bytes32 sId = _getSubscriptionId(subscriber, pId);
        (exist, sdata) = _getSubscriptionData(token, sId);
        require(exist, "IDAv1: index does not exist");
        units = sdata.units;
    }

    function listSubscriptions(
        ISuperToken token,
        address subscriber)
            external
            view
            override
            returns(
                address[] memory publishers,
                uint32[] memory indexIds,
                uint128[] memory unitsList) {
        uint256 slotBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            subscriber,
            SUBSCRIPTION_STATE_SLOT_BITMAP_ID, 1)[0]);
        bool exist;
        SubscriptionData memory sdata;
        // read all slots
        uint nSlots;
        publishers = new address[](MAX_NUM_SLOTS);
        indexIds = new uint32[](MAX_NUM_SLOTS);
        unitsList = new uint128[](MAX_NUM_SLOTS);
        for (uint32 slotId = 0; slotId < MAX_NUM_SLOTS; ++slotId) {
            if ((uint256(slotBitmap >> slotId) & 1) == 0) continue;
            bytes32 pId = token.getAgreementStateSlot(
                address(this),
                subscriber,
                SUBSCRIPTION_STATE_SLOT_DATA_ID_START + slotId, 1)[0];
            bytes32 sId = _getSubscriptionId(subscriber, pId);
            (exist, sdata) = _getSubscriptionData(token, sId);
            require(exist, "IDAv1: subscription does not exist");
            require(sdata.slotId == slotId, "IDAv1: incorrect slot id");
            publishers[nSlots] = sdata.publisher;
            indexIds[nSlots] = sdata.indexId;
            unitsList[nSlots] = sdata.units;
            ++nSlots;
        }
        // resize memory
        assembly {
            mstore(publishers, nSlots)
            mstore(indexIds, nSlots)
            mstore(unitsList, nSlots)
        }
    }

    function _getPublisherId(
        address publisher,
        uint32 indexId)
        private
        pure
        returns (bytes32 pId)
    {
        return keccak256(abi.encode("publisher", publisher, indexId));
    }

    function _getSubscriptionId(
        address subscriber,
        bytes32 pId)
        private
        pure
        returns (bytes32 sId)
    {
        return keccak256(abi.encode("subscription", subscriber, pId));
    }

    function _encodePublisherData(
        PublisherData memory pdata)
        private
        pure
        returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] = bytes32(
            uint256(1 << 128) /* existance bit */ |
            uint256(pdata.indexValue)
        );
        data[1] = bytes32(
            (uint256(pdata.totalUnitsApproved)) |
            (uint256(pdata.totalUnitsPending) << 128)
        );
    }

    function _hasPublisherData(
        ISuperToken token,
        bytes32 pId)
        private
        view
        returns (bool exist)
    {
        bytes32[] memory adata = token.getAgreementData2(address(this), pId, 2);
        uint256 a = uint256(adata[0]);
        exist = a > 0;
    }

    function _getPublisherData(
        ISuperToken token,
        bytes32 pId)
        private
        view
        returns (bool exist, PublisherData memory pdata)
    {
        bytes32[] memory adata = token.getAgreementData2(address(this), pId, 2);
        uint256 a = uint256(adata[0]);
        uint256 b = uint256(adata[1]);
        exist = a > 0;
        if (exist) {
            pdata.indexValue = uint128(a & uint256(int128(-1)));
            pdata.totalUnitsApproved = uint128(b & uint256(int128(-1)));
            pdata.totalUnitsPending = uint128(b >> 128);
        }
    }

    function _encodeSubscriptionData(
        SubscriptionData memory sdata)
        private
        pure
        returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] = bytes32(
            (uint256(sdata.publisher) << (12*8)) |
            (uint256(sdata.indexId) << 32) |
            uint256(sdata.slotId)
        );
        data[1] = bytes32(
            uint256(sdata.indexValue) |
            (uint256(sdata.units) << 128)
        );
    }

    function _hasSubscriptionData(
        ISuperToken token,
        bytes32 sId)
        private
        view
        returns (bool exist)
    {
        bytes32[] memory adata = token.getAgreementData2(address(this), sId, 2);
        uint256 a = uint256(adata[0]);
        exist = a > 0;
    }

    function _getSubscriptionData(
        ISuperToken token,
        bytes32 sId)
        private
        view
        returns (bool exist, SubscriptionData memory sdata)
    {
        bytes32[] memory adata = token.getAgreementData2(address(this), sId, 2);
        uint256 a = uint256(adata[0]);
        uint256 b = uint256(adata[1]);
        exist = a > 0;
        if (exist) {
            sdata.publisher = address(uint160(a >> (12*8)));
            sdata.indexId = uint32((a >> 32) & uint32(int32(-1)));
            sdata.slotId = uint32(a & uint32(int32(-1)));
            sdata.indexValue = uint128(b & uint256(int128(-1)));
            sdata.units = uint128(b >> 128);
        }
    }

    function _findAndFillSlot(
        ISuperToken token,
        address subscriber,
        bytes32 pId)
        private
        returns (uint32 slotId) {
        uint256 slotBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            subscriber,
            SUBSCRIPTION_STATE_SLOT_BITMAP_ID, 1)[0]);
        for (slotId = 0; slotId < MAX_NUM_SLOTS; ++slotId) {
            if ((uint256(slotBitmap >> slotId) & 1) == 0) {
                // update slot data
                bytes32[] memory slotData = new bytes32[](1);
                slotData[0] = pId;
                token.updateAgreementStateSlot(
                    subscriber,
                    SUBSCRIPTION_STATE_SLOT_DATA_ID_START + slotId,
                    slotData);
                // update slot map
                slotData[0] = bytes32(slotBitmap | (1 << uint256(slotId)));
                token.updateAgreementStateSlot(
                    subscriber,
                    SUBSCRIPTION_STATE_SLOT_BITMAP_ID,
                    slotData);
                // update the slots
                break;
            }
        }
    }
}
