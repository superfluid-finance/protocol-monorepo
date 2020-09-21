// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { IInstantDistributionAgreementV1, ISuperToken } from "../interfaces/IInstantDistributionAgreementV1.sol";
import { ISuperfluid } from "../interfaces/ISuperfluid.sol";
import { ContextLibrary } from "../superfluid/ContextLibrary.sol";

contract InstantDistributionAgreementV1 is IInstantDistributionAgreementV1 {

    string private constant _ERR_STR_INDEX_DOES_NOT_EXIST = "IDAv1: index does not exist";
    string private constant _ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST = "IDAv1: subscription does not exist";
    string private constant _ERR_STR_INCORRECT_PUBLISHER = "IDAv1: incorrect publisher";
    string private constant _ERR_STR_INCORRECT_INDEX_ID = "IDAv1: incorrect indexId";
    string private constant _ERR_STR_INCORRECT_SLOT_ID = "IDAv1: incorrect slot id";

    /// @dev Subscriber state slot id for storing subs bitmap
    uint256 private constant _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID = 0;
    /// @dev Publisher state slot id for storing its deposit amount
    uint256 private constant _PUBLISHER_DEPOSIT_STATE_SLOT_ID = 1 << 32;
    /// @dev Subscriber state slot id starting ptoint for subscription data
    uint256 private constant _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START = 1 << 128;

    /// @dev Maximum number of subscriptions a subscriber can have
    uint32 private constant _MAX_NUM_SUBS = 256;
    /// @dev A special id that indicating the subscription is not approved yet
    uint32 private constant _UNALLOCATED_SUB_ID = uint32(int32(-1));

    /// @dev Agreement data for the index
    struct IndexData {
        uint128 indexValue;
        uint128 totalUnitsApproved;
        uint128 totalUnitsPending;
    }

    /// @dev Agreement data for the subscription
    struct SubscriptionData {
        uint32 subId;
        address publisher;
        uint32 indexId;
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
            uint256 deposit,
            uint256 /*owedDeposit*/
        ) {
        bool exist;
        IndexData memory idata;
        SubscriptionData memory sdata;

        // as a subscriber
        // read all slots and calculate the real-time balance
        uint256 subsBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            account,
            _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID, 1)[0]);
        for (uint32 subId = 0; subId < _MAX_NUM_SUBS; ++subId) {
            if ((uint256(subsBitmap >> subId) & 1) == 0) continue;
            bytes32 iId = token.getAgreementStateSlot(
                address(this),
                account,
                _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START + subId, 1)[0];
            bytes32 sId = _getSubscriptionId(account, iId);
            (exist, idata) = _getIndexData(token, iId);
            require(exist, _ERR_STR_INDEX_DOES_NOT_EXIST);
            (exist, sdata) = _getSubscriptionData(token, sId);
            require(exist, _ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST);
            require(sdata.subId == subId, _ERR_STR_INCORRECT_SLOT_ID);
            dynamicBalance += int256(idata.indexValue - sdata.indexValue) * int256(sdata.units);
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
        bytes32 iId = _getPublisherId(publisher, indexId);
        require(!_hasIndexData(token, iId), "IDAv1: index already exists");
        token.createAgreement2(iId, _encodeIndexData(IndexData(0, 0, 0)));

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
        IndexData memory idata;
        bytes32 iId = _getPublisherId(publisher, indexId);
        (exist, idata) = _getIndexData(token, iId);
        if (exist) {
            indexValue = idata.indexValue;
            totalUnitsApproved = idata.totalUnitsApproved;
            totalUnitsPending = idata.totalUnitsPending;
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
        bytes32 iId = _getPublisherId(publisher, indexId);
        (bool exist, IndexData memory idata) = _getIndexData(token, iId);
        require(exist, _ERR_STR_INDEX_DOES_NOT_EXIST);
        require(indexValue >= idata.indexValue, "IDAv1: index value should grow");

        // - settle the publisher balance INSTANT-ly (ding ding ding, IDA)
        //   - adjust static balance directly
        token.settleBalance(
            publisher,
            - int256(indexValue - idata.indexValue) // FIXME int256 safe math
                * int256(idata.totalUnitsApproved));
        //   - adjust the publisher's deposit amount
        _adjustPublisherDeposit(token, publisher,
            int256(indexValue - idata.indexValue) // FIXME int256 safe math
                * int256(idata.totalUnitsPending));
        // adjust the publisher's index data
        idata.indexValue = indexValue;
        token.updateAgreementData2(iId, _encodeIndexData(idata));

        // check account solvency
        require(!token.isAccountInsolvent(publisher), "IDAv1: insufficient balance of publisher");

        // TODO support ctx
        newCtx = ctx;
    }

    /// @dev IInstantDistributionAgreementV1.approveSubscription implementation
    function approveSubscription(
        ISuperToken token,
        address publisher,
        uint32 indexId,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        bool exist;
        IndexData memory idata;
        SubscriptionData memory sdata;
        address subscriber = ContextLibrary.decode(ctx).msgSender;
        bytes32 iId = _getPublisherId(publisher, indexId);
        bytes32 sId = _getSubscriptionId(subscriber, iId);
        (exist, idata) = _getIndexData(token, iId);
        require(exist, _ERR_STR_INDEX_DOES_NOT_EXIST);

        (exist, sdata) = _getSubscriptionData(token, sId);
        if (!exist) {
            sdata = SubscriptionData({
                publisher: publisher,
                indexId: indexId,
                subId: 0,
                units: 0,
                indexValue: idata.indexValue
            });
            // add to subscription list of the subscriber
            sdata.subId = _findAndFillSubsBitmap(token, subscriber, iId);
            token.createAgreement2(sId, _encodeSubscriptionData(sdata));
        } else {
            // sanity check
            require(sdata.publisher == publisher, _ERR_STR_INCORRECT_PUBLISHER);
            require(sdata.indexId == indexId, _ERR_STR_INCORRECT_INDEX_ID);
            // required condition check
            require(sdata.subId == _UNALLOCATED_SUB_ID, "IDAv1: subscription already approved");

            int balanceDelta = int256(idata.indexValue - sdata.indexValue) * int256(sdata.units);

            // update publisher data and adjust publisher's deposits
            idata.totalUnitsApproved += sdata.units; // FIXME safe int256
            idata.totalUnitsPending -= sdata.units;
            token.updateAgreementData2(iId, _encodeIndexData(idata));
            _adjustPublisherDeposit(token, publisher, -balanceDelta);
            token.settleBalance(publisher, -balanceDelta);

            // update subscription data and adjust subscriber's balance
            token.settleBalance(subscriber, balanceDelta);
            sdata.indexValue = idata.indexValue;
            sdata.subId = _findAndFillSubsBitmap(token, subscriber, iId);
            token.updateAgreementData2(sId, _encodeSubscriptionData(sdata));
        }

        // TODO support ctx
        newCtx = ctx;
    }

    /// @dev IInstantDistributionAgreementV1.updateSubscription implementation
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
        IndexData memory idata;
        address publisher = ContextLibrary.decode(ctx).msgSender;
        bytes32 iId = _getPublisherId(publisher, indexId);
        bytes32 sId = _getSubscriptionId(subscriber, iId);
        (exist, idata) = _getIndexData(token, iId);
        require(exist, _ERR_STR_INDEX_DOES_NOT_EXIST);
        (exist, sdata) = _getSubscriptionData(token, sId);

        // update publisher data
        if (exist && sdata.subId != _UNALLOCATED_SUB_ID) {
            // if the subscription exist, update the approved units amount

            // sanity check
            require(sdata.publisher == publisher, _ERR_STR_INCORRECT_PUBLISHER);
            require(sdata.indexId == indexId, _ERR_STR_INCORRECT_INDEX_ID);

            // update total units
            if (units > sdata.units) {
                idata.totalUnitsApproved += units - sdata.units; // FIXME safe128
            } else {
                idata.totalUnitsApproved -= units - sdata.units; // FIXME safe128
            }
            token.updateAgreementData2(iId, _encodeIndexData(idata));
        } else {
            // if the subscription does not exist, update the pending units amount
            if (!exist) {
                // create unallocated subscription
                sdata = SubscriptionData({
                    publisher: publisher,
                    indexId: indexId,
                    subId: _UNALLOCATED_SUB_ID,
                    units: units,
                    indexValue: idata.indexValue
                });
                token.createAgreement2(sId, _encodeSubscriptionData(sdata));
                idata.totalUnitsPending += units;
            } else {
                // update pending subscription units of the publisher
                if (units > sdata.units) {
                    idata.totalUnitsPending += units - sdata.units; // FIXME safe128
                } else {
                    idata.totalUnitsPending -= units - sdata.units; // FIXME safe128
                }
            }
            token.updateAgreementData2(iId, _encodeIndexData(idata));
        }

        int256 balanceDelta = int256(idata.indexValue - sdata.indexValue) * int256(sdata.units);

        // adjust publisher's deposit and balances if subscription is pending
        if (sdata.subId == _UNALLOCATED_SUB_ID) {
            _adjustPublisherDeposit(token, publisher, -balanceDelta);
            token.settleBalance(publisher, -balanceDelta);
        }

        // settle subscriber static balance
        token.settleBalance(subscriber, balanceDelta);
        // update subscription data and settle subscriber's balance
        sdata.indexValue = idata.indexValue;
        sdata.units = units;
        token.updateAgreementData2(sId, _encodeSubscriptionData(sdata));

        // check account solvency
        require(!token.isAccountInsolvent(publisher), "IDAv1: insufficient balance of publisher");

        // TODO callback support
        newCtx = ctx;
    }

    /// @dev IInstantDistributionAgreementV1.getSubscription implementation
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
        IndexData memory idata;
        SubscriptionData memory sdata;
        bytes32 iId = _getPublisherId(publisher, indexId);
        (exist, idata) = _getIndexData(token, iId);
        require(exist, _ERR_STR_INDEX_DOES_NOT_EXIST);
        bytes32 sId = _getSubscriptionId(subscriber, iId);
        (exist, sdata) = _getSubscriptionData(token, sId);
        require(exist, _ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST);
        approved = sdata.subId != _UNALLOCATED_SUB_ID;
        units = sdata.units;
        pendingDistribution = approved ? 0 :
            uint256(idata.indexValue - sdata.indexValue) * uint256(sdata.units);
    }

    /// @dev IInstantDistributionAgreementV1.listSubscriptions implementation
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
        uint256 subsBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            subscriber,
            _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID, 1)[0]);
        bool exist;
        SubscriptionData memory sdata;
        // read all slots
        uint nSlots;
        publishers = new address[](_MAX_NUM_SUBS);
        indexIds = new uint32[](_MAX_NUM_SUBS);
        unitsList = new uint128[](_MAX_NUM_SUBS);
        for (uint32 subId = 0; subId < _MAX_NUM_SUBS; ++subId) {
            if ((uint256(subsBitmap >> subId) & 1) == 0) continue;
            bytes32 iId = token.getAgreementStateSlot(
                address(this),
                subscriber,
                _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START + subId, 1)[0];
            bytes32 sId = _getSubscriptionId(subscriber, iId);
            (exist, sdata) = _getSubscriptionData(token, sId);
            require(exist, _ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST);
            require(sdata.subId == subId, _ERR_STR_INCORRECT_SLOT_ID);
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

    /// @dev IInstantDistributionAgreementV1.deleteSubscription implementation
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
        IndexData memory idata;
        address sender = ContextLibrary.decode(ctx).msgSender;
        require(sender == publisher || sender == subscriber, "IDAv1: operation not allowed");
        bytes32 iId = _getPublisherId(publisher, indexId);
        bytes32 sId = _getSubscriptionId(subscriber, iId);
        (exist, idata) = _getIndexData(token, iId);
        require(exist, _ERR_STR_INDEX_DOES_NOT_EXIST);
        (exist, sdata) = _getSubscriptionData(token, sId);
        require(exist, _ERR_STR_SUBSCRIPTION_DOES_NOT_EXIST);

        int256 balanceDelta = int256(idata.indexValue - sdata.indexValue) * int256(sdata.units);

        // update publisher index agreement data
        if (sdata.subId != _UNALLOCATED_SUB_ID) {
            idata.totalUnitsApproved -= sdata.units; // FIXME safe128
        } else {
            idata.totalUnitsPending -= sdata.units; // FIXME safe128
        }
        token.updateAgreementData2(iId, _encodeIndexData(idata));

        // terminate subscription agreement data
        token.terminateAgreement2(sId, 2);
        // remove subscription from subscriber's bitmap
        if (sdata.subId != _UNALLOCATED_SUB_ID) {
            _clearSubsBitmap(token, subscriber, sdata);
        }

        // move from publisher's deposit to static balance
        if (sdata.subId == _UNALLOCATED_SUB_ID) {
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
        returns (bytes32 iId)
    {
        return keccak256(abi.encode("publisher", publisher, indexId));
    }

    function _getSubscriptionId(
        address subscriber,
        bytes32 iId)
        private
        pure
        returns (bytes32 sId)
    {
        return keccak256(abi.encode("subscription", subscriber, iId));
    }

    // # Index data operations
    //
    // Data packing:
    //
    // WORD 1: | existence bit  | indexValue |
    //         | 128b           | 128b       |
    // WORD 2: | totalUnitsPending | totalUnitsApproved |
    //         | 128b              | 12b                |

    function _encodeIndexData(
        IndexData memory idata)
        private
        pure
        returns (bytes32[] memory data) {
        data = new bytes32[](2);
        data[0] = bytes32(
            uint256(1 << 128) /* existance bit */ |
            uint256(idata.indexValue)
        );
        data[1] = bytes32(
            (uint256(idata.totalUnitsApproved)) |
            (uint256(idata.totalUnitsPending) << 128)
        );
    }

    function _hasIndexData(
        ISuperToken token,
        bytes32 iId)
        private
        view
        returns (bool exist)
    {
        bytes32[] memory adata = token.getAgreementData2(address(this), iId, 2);
        uint256 a = uint256(adata[0]);
        exist = a > 0;
    }

    function _getIndexData(
        ISuperToken token,
        bytes32 iId)
        private
        view
        returns (bool exist, IndexData memory idata)
    {
        bytes32[] memory adata = token.getAgreementData2(address(this), iId, 2);
        uint256 a = uint256(adata[0]);
        uint256 b = uint256(adata[1]);
        exist = a > 0;
        if (exist) {
            idata.indexValue = uint128(a & uint256(int128(-1)));
            idata.totalUnitsApproved = uint128(b & uint256(int128(-1)));
            idata.totalUnitsPending = uint128(b >> 128);
        }
    }


    // # Publisher's deposit amount
    //
    // It is stored in state slot in one word

    function _getPublisherDeposit(
        ISuperToken token,
        address publisher
    )
        private
        view
        returns (uint256)
    {
        bytes32[] memory data = token.getAgreementStateSlot(
            address(this),
            publisher,
            _PUBLISHER_DEPOSIT_STATE_SLOT_ID,
            1);
        return uint256(data[0]);
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
            _PUBLISHER_DEPOSIT_STATE_SLOT_ID,
            1);
        data[0] = bytes32(int256(data[0]) + delta);
        token.updateAgreementStateSlot(
            publisher,
            _PUBLISHER_DEPOSIT_STATE_SLOT_ID,
            data);
    }

    // # Subscription data operations
    //
    // Data packing:
    //
    // WORD 1: | publisher | RESERVED | indexId | subId |
    //         | 160b      | 32b      | 32b     | 32b   |
    // WORD 2: | units | indexValue |
    //         | 128b  | 128b       |

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
            uint256(sdata.subId)
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
            sdata.subId = uint32(a & uint32(int32(-1)));
            sdata.indexValue = uint128(b & uint256(int128(-1)));
            sdata.units = uint128(b >> 128);
        }
    }

    // # Subscription bitmap operations
    //
    // ## Subscription bitmap state slot
    //
    // slotId: _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID)
    //
    // Subscriber can store up to _MAX_NUM_SUBS amount of subscriptions.
    // For each subscription it is filled one bit in the subscription bitmap.
    //
    // ## Subscription reference state slots
    //
    // slotId: _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START + i)
    //
    // It stores the index data ID.

    function _findAndFillSubsBitmap(
        ISuperToken token,
        address subscriber,
        bytes32 iId)
        private
        returns (uint32 subId) {
        uint256 subsBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            subscriber,
            _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID, 1)[0]);
        for (subId = 0; subId < _MAX_NUM_SUBS; ++subId) {
            if ((uint256(subsBitmap >> subId) & 1) == 0) {
                // update slot data
                bytes32[] memory slotData = new bytes32[](1);
                slotData[0] = iId;
                token.updateAgreementStateSlot(
                    subscriber,
                    _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START + subId,
                    slotData);
                // update slot map
                slotData[0] = bytes32(subsBitmap | (1 << uint256(subId)));
                token.updateAgreementStateSlot(
                    subscriber,
                    _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID,
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
            _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID, 1)[0]);
        bytes32[] memory slotData = new bytes32[](1);
        slotData[0] = bytes32(subsBitmap ^ (1 << uint256(sdata.subId)));
        token.updateAgreementStateSlot(
            subscriber,
            _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID,
            slotData);
    }
}
