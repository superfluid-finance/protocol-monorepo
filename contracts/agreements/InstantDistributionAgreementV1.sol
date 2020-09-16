// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { IInstantDistributionAgreementV1, ISuperToken } from "../interfaces/IInstantDistributionAgreementV1.sol";
import { ISuperfluid } from "../interfaces/ISuperfluid.sol";
import { ContextLibrary } from "../superfluid/ContextLibrary.sol";

contract InstantDistributionAgreementV1 is IInstantDistributionAgreementV1 {

    uint32 public constant MAX_NUM_SLOTS = 256;

    string public constant ERR_STR_INDEX_DOES_NOT_EXIST = "IDAv1: index does not exist";
    string public constant ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST = "IDAv1: subscription does not exist";
    string public constant ERR_STR_INCORRECT_PUBLISHER = "IDAv1: incorrect publisher";
    string public constant ERR_STR_INCORRECT_INDEX_ID = "IDAv1: incorrect indexId";
    string public constant ERR_STR_INCORRECT_SLOT_ID = "IDAv1: incorrect slot id";

    uint256 public constant SUBSCRIBER_BITMAP_STATE_SLOT_ID = 0;
    uint256 public constant PUBLISHER_DEPOSIT_STATE_SLOT_ID = 1 << 32;
    uint256 public constant SUBSCRIBER_DATA_STATE_SLOT_ID_START = 1 << 128;
    uint32 public constant UNALLOCATED_SLOT_ID = uint32(int32(-1));

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

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        ISuperToken token,
        address account,
        bytes calldata /* state */,
        uint256 /*time*/
    )
        external
        view
        override
        returns (
            int256 dynamicBalance,
            int256 deposit,
            int256 /*owedDeposit*/
        ) {
        bool exist;
        PublisherData memory pdata;
        SubscriptionData memory sdata;

        // as a subscriber
        // read all slots and calculate the real-time balance
        uint256 slotBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            account,
            SUBSCRIBER_BITMAP_STATE_SLOT_ID, 1)[0]);
        for (uint32 slotId = 0; slotId < MAX_NUM_SLOTS; ++slotId) {
            if ((uint256(slotBitmap >> slotId) & 1) == 0) continue;
            bytes32 pId = token.getAgreementStateSlot(
                address(this),
                account,
                SUBSCRIBER_DATA_STATE_SLOT_ID_START + slotId, 1)[0];
            bytes32 sId = _getSubscriptionId(account, pId);
            (exist, pdata) = _getPublisherData(token, pId);
            require(exist, ERR_STR_INDEX_DOES_NOT_EXIST);
            (exist, sdata) = _getSubscriptionData(token, sId);
            require(exist, ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST);
            require(sdata.slotId == slotId, ERR_STR_INCORRECT_SLOT_ID);
            dynamicBalance += int256(pdata.indexValue - sdata.indexValue) * int256(sdata.units);
        }

        // as a publisher
        // calculate the deposits due to pending subscriptions
        deposit = _getPublisherDeposit(token, account);
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

    /// @dev IInstantDistributionAgreementV1.createIndex implementation
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

        // TODO ctx
        newCtx = ctx;
    }

    /// @dev IInstantDistributionAgreementV1.getIndex implementation
    function getIndex(
        ISuperToken token,
        address publisher,
        uint32 indexId)
            external
            view
            override
            returns(
                bool exist,
                uint128 indexValue,
                uint128 totalUnitsApproved,
                uint128 totalUnitsPending) {
        PublisherData memory pdata;
        bytes32 pId = _getPublisherId(publisher, indexId);
        (exist, pdata) = _getPublisherData(token, pId);
        if (exist) {
            indexValue = pdata.indexValue;
            totalUnitsApproved = pdata.totalUnitsApproved;
            totalUnitsPending = pdata.totalUnitsPending;
        }
    }

    /// @dev IInstantDistributionAgreementV1.updateIndex implementation
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
        require(exist, ERR_STR_INDEX_DOES_NOT_EXIST);
        require(indexValue >= pdata.indexValue, "IDAv1: index value should grow");

        // - settle the publisher balance INSTANT-ly (ding ding ding, IDA)
        //   - adjust static balance directly
        token.settleBalance(
            publisher,
            - int256(indexValue - pdata.indexValue) // FIXME int256 safe math
                * int256(pdata.totalUnitsApproved));
        //   - adjust the publisher's deposit amount
        _adjustPublisherDeposit(token, publisher,
            int256(indexValue - pdata.indexValue) // FIXME int256 safe math
                * int256(pdata.totalUnitsPending));
        // adjust the publisher's index data
        pdata.indexValue = indexValue;
        token.updateAgreementData2(pId, _encodePublisherData(pdata));

        // check account solvency
        require(!token.isAccountInsolvent(publisher), "IDAv1: insufficient balance of publisher");

        // TODO support ctx
        newCtx = ctx;
    }

    function approveSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        bool exist;
        PublisherData memory pdata;
        SubscriptionData memory sdata;
        address subscriber = ContextLibrary.decode(ctx).msgSender;
        bytes32 pId = _getPublisherId(publisher, indexId);
        bytes32 sId = _getSubscriptionId(subscriber, pId);
        (exist, pdata) = _getPublisherData(token, pId);
        require(exist, ERR_STR_INDEX_DOES_NOT_EXIST);

        (exist, sdata) = _getSubscriptionData(token, sId);
        if (!exist) {
            sdata = SubscriptionData({
                publisher: publisher,
                indexId: indexId,
                slotId: 0,
                units: 0,
                indexValue: pdata.indexValue
            });
            // add to subscription list of the subscriber
            sdata.slotId = _findAndFillSlot(token, subscriber, pId);
            token.createAgreement2(sId, _encodeSubscriptionData(sdata));
        } else {
            // sanity check
            require(sdata.publisher == publisher, ERR_STR_INCORRECT_PUBLISHER);
            require(sdata.indexId == indexId, ERR_STR_INCORRECT_INDEX_ID);
            // required condition check
            require(sdata.slotId == UNALLOCATED_SLOT_ID, "IDAv1: subscription already approved");

            int balanceDelta = int256(pdata.indexValue - sdata.indexValue) * int256(sdata.units);

            // update publisher data and adjust publisher's deposits
            pdata.totalUnitsApproved += sdata.units; // FIXME safe int256
            pdata.totalUnitsPending -= sdata.units;
            token.updateAgreementData2(pId, _encodePublisherData(pdata));
            _adjustPublisherDeposit(token, publisher, -balanceDelta);
            token.settleBalance(publisher, -balanceDelta);

            // update subscription data and adjust subscriber's balance
            token.settleBalance(subscriber, balanceDelta);
            sdata.indexValue = pdata.indexValue;
            sdata.slotId = _findAndFillSlot(token, subscriber, pId);
            token.updateAgreementData2(sId, _encodeSubscriptionData(sdata));
        }

        // TODO support ctx
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
        require(exist, ERR_STR_INDEX_DOES_NOT_EXIST);
        (exist, sdata) = _getSubscriptionData(token, sId);

        // update publisher data
        if (exist && sdata.slotId != UNALLOCATED_SLOT_ID) {
            // if the subscription exist, update the approved units amount

            // sanity check
            require(sdata.publisher == publisher, ERR_STR_INCORRECT_PUBLISHER);
            require(sdata.indexId == indexId, ERR_STR_INCORRECT_INDEX_ID);

            // update total units
            if (units > sdata.units) {
                pdata.totalUnitsApproved += units - sdata.units; // FIXME safe128
            } else {
                pdata.totalUnitsApproved -= units - sdata.units; // FIXME safe128
            }
            token.updateAgreementData2(pId, _encodePublisherData(pdata));
        } else {
            // if the subscription does not exist, update the pending units amount
            if (!exist) {
                // create unallocated subscription
                sdata = SubscriptionData({
                    publisher: publisher,
                    indexId: indexId,
                    slotId: UNALLOCATED_SLOT_ID,
                    units: units,
                    indexValue: pdata.indexValue
                });
                token.createAgreement2(sId, _encodeSubscriptionData(sdata));
                pdata.totalUnitsPending += units;
            } else {
                // update pending subscription units of the publisher
                if (units > sdata.units) {
                    pdata.totalUnitsPending += units - sdata.units; // FIXME safe128
                } else {
                    pdata.totalUnitsPending -= units - sdata.units; // FIXME safe128
                }
            }
            token.updateAgreementData2(pId, _encodePublisherData(pdata));
        }

        int256 balanceDelta = int256(pdata.indexValue - sdata.indexValue) * int256(sdata.units);

        // adjust publisher's deposit and balances if subscription is pending
        if (sdata.slotId == UNALLOCATED_SLOT_ID) {
            _adjustPublisherDeposit(token, publisher, -balanceDelta);
            token.settleBalance(publisher, -balanceDelta);
        }

        // settle subscriber static balance
        token.settleBalance(subscriber, balanceDelta);
        // update subscription data and settle subscriber's balance
        sdata.indexValue = pdata.indexValue;
        sdata.units = units;
        token.updateAgreementData2(sId, _encodeSubscriptionData(sdata));

        // check account solvency
        require(!token.isAccountInsolvent(publisher), "IDAv1: insufficient balance of publisher");

        // TODO callback support
        newCtx = ctx;
    }

    function getSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber)
            external
            view
            override
            returns (
                bool approved,
                uint128 units,
                uint256 pendingDistribution
            ) {
        bool exist;
        PublisherData memory pdata;
        SubscriptionData memory sdata;
        bytes32 pId = _getPublisherId(publisher, indexId);
        (exist, pdata) = _getPublisherData(token, pId);
        require(exist, ERR_STR_INDEX_DOES_NOT_EXIST);
        bytes32 sId = _getSubscriptionId(subscriber, pId);
        (exist, sdata) = _getSubscriptionData(token, sId);
        require(exist, ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST);
        approved = sdata.slotId != UNALLOCATED_SLOT_ID;
        units = sdata.units;
        pendingDistribution = approved ? 0 :
            uint256(pdata.indexValue - sdata.indexValue) * uint256(sdata.units);
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
            SUBSCRIBER_BITMAP_STATE_SLOT_ID, 1)[0]);
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
                SUBSCRIBER_DATA_STATE_SLOT_ID_START + slotId, 1)[0];
            bytes32 sId = _getSubscriptionId(subscriber, pId);
            (exist, sdata) = _getSubscriptionData(token, sId);
            require(exist, ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST);
            require(sdata.slotId == slotId, ERR_STR_INCORRECT_SLOT_ID);
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

    function deleteSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        bool exist;
        SubscriptionData memory sdata;
        PublisherData memory pdata;
        address sender = ContextLibrary.decode(ctx).msgSender;
        require(sender == publisher || sender == subscriber, "IDAv1: operation not allowed");
        bytes32 pId = _getPublisherId(publisher, indexId);
        bytes32 sId = _getSubscriptionId(subscriber, pId);
        (exist, pdata) = _getPublisherData(token, pId);
        require(exist, ERR_STR_INDEX_DOES_NOT_EXIST);
        (exist, sdata) = _getSubscriptionData(token, sId);
        require(exist, ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST);

        int256 balanceDelta = int256(pdata.indexValue - sdata.indexValue) * int256(sdata.units);

        // update publisher index agreement data
        if (sdata.slotId != UNALLOCATED_SLOT_ID) {
            pdata.totalUnitsApproved -= sdata.units; // FIXME safe128
        } else {
            pdata.totalUnitsPending -= sdata.units; // FIXME safe128
        }
        token.updateAgreementData2(pId, _encodePublisherData(pdata));

        // terminate subscription agreement data
        token.terminateAgreement2(sId, 2);
        // remove subscription from subscriber's bitmap
        if (sdata.slotId != UNALLOCATED_SLOT_ID) {
            _clearSubsBitmap(token, subscriber, sdata);
        }

        // move from publisher's deposit to static balance
        if (sdata.slotId == UNALLOCATED_SLOT_ID) {
            _adjustPublisherDeposit(token, publisher, -balanceDelta);
            token.settleBalance(publisher, -balanceDelta);
        }

        // settle subscriber static balance
        token.settleBalance(subscriber, balanceDelta);

        // TODO callback support
        newCtx = ctx;
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

    function _getPublisherDeposit(
        ISuperToken token,
        address publisher
    )
        private
        view
        returns (int256)
    {
        bytes32[] memory data = token.getAgreementStateSlot(
            address(this),
            publisher,
            PUBLISHER_DEPOSIT_STATE_SLOT_ID,
            1);
        return int256(data[0]);
    }

    function _adjustPublisherDeposit(
        ISuperToken token,
        address publisher,
        int256 delta
    )
        private
    {
        if (delta == 0) return;
        bytes32[] memory data = token.getAgreementStateSlot(
            address(this),
            publisher,
            PUBLISHER_DEPOSIT_STATE_SLOT_ID,
            1);
        data[0] = bytes32(int256(data[0]) + delta);
        token.updateAgreementStateSlot(
            publisher,
            PUBLISHER_DEPOSIT_STATE_SLOT_ID,
            data);
    }

    function _encodeSubscriptionData(
        SubscriptionData memory sdata)
        private
        pure
        returns (bytes32[] memory data)
    {
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
            SUBSCRIBER_BITMAP_STATE_SLOT_ID, 1)[0]);
        for (slotId = 0; slotId < MAX_NUM_SLOTS; ++slotId) {
            if ((uint256(slotBitmap >> slotId) & 1) == 0) {
                // update slot data
                bytes32[] memory slotData = new bytes32[](1);
                slotData[0] = pId;
                token.updateAgreementStateSlot(
                    subscriber,
                    SUBSCRIBER_DATA_STATE_SLOT_ID_START + slotId,
                    slotData);
                // update slot map
                slotData[0] = bytes32(slotBitmap | (1 << uint256(slotId)));
                token.updateAgreementStateSlot(
                    subscriber,
                    SUBSCRIBER_BITMAP_STATE_SLOT_ID,
                    slotData);
                // update the slots
                break;
            }
        }
    }

    function _clearSubsBitmap(
        ISuperToken token,
        address subscriber,
        SubscriptionData memory sdata
    )
        private {
        uint256 subsBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            subscriber,
            SUBSCRIBER_BITMAP_STATE_SLOT_ID, 1)[0]);
        bytes32[] memory slotData = new bytes32[](1);
        slotData[0] = bytes32(subsBitmap ^ (1 << uint256(sdata.slotId)));
        token.updateAgreementStateSlot(
            subscriber,
            SUBSCRIBER_BITMAP_STATE_SLOT_ID,
            slotData);
    }
}
