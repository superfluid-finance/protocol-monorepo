// SPDX-License-Identifier: AGPLv3
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.6;

import {
    IInstantDistributionAgreementV1,
    ISuperfluidToken
} from "../interfaces/agreements/IInstantDistributionAgreementV1.sol";
import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperApp,
    SuperAppDefinitions
}
from "../interfaces/superfluid/ISuperfluid.sol";
import { AgreementBase } from "./AgreementBase.sol";

import { UInt128SafeMath } from "../utils/UInt128SafeMath.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/SafeCast.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";


contract InstantDistributionAgreementV1 is
    AgreementBase,
    IInstantDistributionAgreementV1
{

    /*
        E_NO_INDEX - index does not exist
        E_INDEX_EXISTS - index already exists
        E_INDEX_GROW - index value should grow
        E_LOW_BALANCE - insufficient balance
        E_SUBS_APPROVED subscription already approved
        E_NO_SUBS - subscription does not exist
        E_NOT_ALLOWED - operation not allowed
     */

    using SafeMath for uint256;
    using SafeCast for uint256;
    using UInt128SafeMath for uint128;
    using SignedSafeMath for int256;

    /// @dev Subscriber state slot id for storing subs bitmap
    uint256 private constant _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID = 0;
    /// @dev Publisher state slot id for storing its deposit amount
    uint256 private constant _PUBLISHER_DEPOSIT_STATE_SLOT_ID = 1 << 32;
    /// @dev Subscriber state slot id starting ptoint for subscription data
    uint256 private constant _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START = 1 << 128;

    /// @dev Maximum number of subscriptions a subscriber can have
    uint32 private constant _MAX_NUM_SUBS = 256;
    /// @dev A special id that indicating the subscription is not approved yet
    uint32 private constant _UNALLOCATED_SUB_ID = type(uint32).max;

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

    // Stack variables to avoid stack too deep errors in some functions
    // solhint-disable-next-line contract-name-camelcase
    struct _StackVars {
        bool exist;
        bytes32 iId;
        bytes32 sId;
        IndexData idata;
        SubscriptionData sdata;
        AgreementLibrary.CallbackInputs cbStates;
        bytes cbdata;
    }

    /**************************************************************************
     * ISuperAgreement interface
     *************************************************************************/

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        ISuperfluidToken token,
        address account,
        uint256 /*time*/
    )
        external view override
        returns (
            int256 dynamicBalance,
            uint256 deposit,
            uint256 owedDeposit
        )
    {
        bool exist;
        IndexData memory idata;
        SubscriptionData memory sdata;

        // as a subscriber
        // read all subs and calculate the real-time balance
        bytes32[] memory sidList = _listSubscriptionIds(token, account);
        for (uint32 subId = 0; subId < sidList.length; ++subId) {
            bytes32 sId = sidList[subId];
            (exist, sdata) = _getSubscriptionData(token, sId);
            //require(exist, "IDA: E_NO_SUBS");
            bytes32 iId = token.getAgreementStateSlot(
                address(this),
                account,
                _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START + subId, 1)[0];
            (exist, idata) = _getIndexData(token, iId);
            //require(exist, "IDA: E_NO_INDEX");
            //assert(sdata.subId == subId);
            dynamicBalance = dynamicBalance.add(
                int256(idata.indexValue - sdata.indexValue) * int256(sdata.units)
            );
        }

        // as a publisher
        // calculate the deposits due to pending subscriptions
        deposit = _getPublisherDeposit(token, account);
        owedDeposit = 0;
    }

    /**************************************************************************
     * Index operations
     *************************************************************************/

    /// @dev IInstantDistributionAgreementV1.createIndex implementation
    function createIndex(
        ISuperfluidToken token,
        uint32 indexId,
        bytes calldata ctx
    )
        external override
        returns(bytes memory newCtx)
    {
        ISuperfluid.Context memory context = AgreementLibrary.authorizeTokenAccess(token, ctx);
        address publisher = context.msgSender;
        bytes32 iId = _getPublisherId(publisher, indexId);
        require(!_hasIndexData(token, iId), "IDA: E_INDEX_EXISTS");

        token.createAgreement(iId, _encodeIndexData(IndexData(0, 0, 0)));

        emit IndexCreated(token, publisher, indexId, context.userData);

        // nothing to be recorded so far
        newCtx = ctx;
    }

    /// @dev IInstantDistributionAgreementV1.getIndex implementation
    function getIndex(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId
    )
        external view override
        returns (
            bool exist,
            uint128 indexValue,
            uint128 totalUnitsApproved,
            uint128 totalUnitsPending)
    {
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
        ISuperfluidToken token,
        uint32 indexId,
        uint128 indexValue,
        bytes calldata ctx
    )
        external override
        returns(bytes memory newCtx)
    {
        ISuperfluid.Context memory context = AgreementLibrary.authorizeTokenAccess(token, ctx);
        address publisher = context.msgSender;
        bytes32 iId = _getPublisherId(publisher, indexId);
        (bool exist, IndexData memory idata) = _getIndexData(token, iId);
        require(exist, "IDA: E_NO_INDEX");
        require(indexValue >= idata.indexValue, "IDA: E_INDEX_GROW");

        _updateIndex(token, publisher, indexId, iId, idata, indexValue, context.userData);

        // nothing to be recorded so far
        newCtx = ctx;
    }

    function distribute(
        ISuperfluidToken token,
        uint32 indexId,
        uint256 amount,
        bytes calldata ctx
    )
        external override
        returns(bytes memory newCtx)
    {
        ISuperfluid.Context memory context = AgreementLibrary.authorizeTokenAccess(token, ctx);
        address publisher = context.msgSender;
        bytes32 iId = _getPublisherId(publisher, indexId);
        (bool exist, IndexData memory idata) = _getIndexData(token, iId);
        require(exist, "IDA: E_NO_INDEX");

        uint128 indexDelta = (
            amount /
            uint256(idata.totalUnitsApproved + idata.totalUnitsPending)
        ).toUint128();
        _updateIndex(token, publisher, indexId, iId, idata, idata.indexValue + indexDelta, context.userData);

        // nothing to be recorded so far
        newCtx = ctx;
    }

    function _updateIndex(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        bytes32 iId,
        IndexData memory idata,
        uint128 indexValue,
        bytes memory userData
    )
        private
    {
        // - settle the publisher balance INSTANT-ly (ding ding ding, IDA)
        //   - adjust static balance directly
        token.settleBalance(publisher,
            (-int256(indexValue - idata.indexValue)).mul(int256(idata.totalUnitsApproved)));
        //   - adjust the publisher's deposit amount
        _adjustPublisherDeposit(token, publisher,
            int256(indexValue - idata.indexValue).mul(int256(idata.totalUnitsPending)));
        // adjust the publisher's index data
        idata.indexValue = indexValue;
        token.updateAgreementData(iId, _encodeIndexData(idata));

        emit IndexUpdated(
            token,
            publisher,
            indexId,
            indexValue,
            idata.totalUnitsPending,
            idata.totalUnitsApproved,
            userData);

        // check account solvency
        require(!token.isAccountCriticalNow(publisher), "IDA: E_LOW_BALANCE");
    }

    function calculateDistribution(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        uint256 amount
    )
        external view override
        returns(
            uint256 actualAmount,
            uint128 newIndexValue)
    {
        bytes32 iId = _getPublisherId(publisher, indexId);
        (bool exist, IndexData memory idata) = _getIndexData(token, iId);
        require(exist, "IDA: E_NO_INDEX");

        uint256 totalUnits = uint256(idata.totalUnitsApproved + idata.totalUnitsPending);
        uint128 indexDelta = (amount / totalUnits).toUint128();
        newIndexValue = idata.indexValue.add(indexDelta, "IDA: E_OVERFLOW");
        actualAmount = uint256(indexDelta).mul(totalUnits);
    }

    /**************************************************************************
     * Subscription operations
     *************************************************************************/

    /// @dev IInstantDistributionAgreementV1.approveSubscription implementation
    function approveSubscription(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        bytes calldata ctx
    )
        external override
        returns(bytes memory newCtx)
    {
        _StackVars memory vars;
        ISuperfluid.Context memory context = AgreementLibrary.authorizeTokenAccess(token, ctx);
        address subscriber = context.msgSender;
        vars.iId = _getPublisherId(publisher, indexId);
        vars.sId = _getSubscriptionId(subscriber, vars.iId);
        (vars.exist, vars.idata) = _getIndexData(token, vars.iId);
        require(vars.exist, "IDA: E_NO_INDEX");
        (vars.exist, vars.sdata) = _getSubscriptionData(token, vars.sId);
        if (vars.exist) {
            // sanity check
            assert(vars.sdata.publisher == publisher);
            assert(vars.sdata.indexId == indexId);
            // required condition check
            require(vars.sdata.subId == _UNALLOCATED_SUB_ID, "IDA: E_SUBS_APPROVED");
        }

        newCtx = ctx;
        vars.cbStates = AgreementLibrary.createCallbackInputs(
            token,
            publisher,
            vars.sId,
            "");

        if (!vars.exist) {
            vars.cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP;
            vars.cbdata = AgreementLibrary.callAppBeforeCallback(vars.cbStates, newCtx);

            vars.sdata = SubscriptionData({
                publisher: publisher,
                indexId: indexId,
                subId: 0,
                units: 0,
                indexValue: vars.idata.indexValue
            });
            // add to subscription list of the subscriber
            vars.sdata.subId = _findAndFillSubsBitmap(token, subscriber, vars.iId);
            token.createAgreement(vars.sId, _encodeSubscriptionData(vars.sdata));

            vars.cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP;
            AgreementLibrary.callAppAfterCallback(vars.cbStates, vars.cbdata, newCtx);
        } else {
            vars.cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP;
            vars.cbdata = AgreementLibrary.callAppBeforeCallback(vars.cbStates, newCtx);

            int balanceDelta = int256(vars.idata.indexValue - vars.sdata.indexValue) * int256(vars.sdata.units);

            // update publisher data and adjust publisher's deposits
            vars.idata.totalUnitsApproved += vars.sdata.units;
            vars.idata.totalUnitsPending -= vars.sdata.units;
            token.updateAgreementData(vars.iId, _encodeIndexData(vars.idata));
            _adjustPublisherDeposit(token, publisher, -balanceDelta);
            token.settleBalance(publisher, -balanceDelta);

            // update subscription data and adjust subscriber's balance
            token.settleBalance(subscriber, balanceDelta);
            vars.sdata.indexValue = vars.idata.indexValue;
            vars.sdata.subId = _findAndFillSubsBitmap(token, subscriber, vars.iId);
            token.updateAgreementData(vars.sId, _encodeSubscriptionData(vars.sdata));

            vars.cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP;
            AgreementLibrary.callAppAfterCallback(vars.cbStates, vars.cbdata, newCtx);
        }

        // can index up to three words, hence splitting into two events from publisher or subscriber's view.
        emit IndexSubscribed(token, publisher, indexId, subscriber, context.userData);
        emit SubscriptionApproved(token, subscriber, publisher, indexId, context.userData);
    }

    /// @dev IInstantDistributionAgreementV1.updateSubscription implementation
    function updateSubscription(
        ISuperfluidToken token,
        uint32 indexId,
        address subscriber,
        uint128 units,
        bytes calldata ctx
    )
        external override
        returns(bytes memory newCtx)
    {
        _StackVars memory vars;
        ISuperfluid.Context memory context = AgreementLibrary.authorizeTokenAccess(token, ctx);
        address publisher = context.msgSender;
        bytes32 iId = _getPublisherId(publisher, indexId);
        bytes32 sId = _getSubscriptionId(subscriber, iId);
        (vars.exist, vars.idata) = _getIndexData(token, iId);
        require(vars.exist, "IDA: E_NO_INDEX");
        (vars.exist, vars.sdata) = _getSubscriptionData(token, sId);
        if (vars.exist) {
            // sanity check
            assert(vars.sdata.publisher == publisher);
            assert(vars.sdata.indexId == indexId);
        }

        vars.cbStates = AgreementLibrary.createCallbackInputs(
            token,
            subscriber,
            sId,
            "");
        newCtx = ctx;

        // before-hook callback
        if (vars.exist) {
            vars.cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP;
            vars.cbdata = AgreementLibrary.callAppBeforeCallback(vars.cbStates, newCtx);
        } else {
            vars.cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP;
            vars.cbdata = AgreementLibrary.callAppBeforeCallback(vars.cbStates, newCtx);
        }

        // update publisher data
        if (vars.exist && vars.sdata.subId != _UNALLOCATED_SUB_ID) {
            // if the subscription exist and not approved, update the approved units amount

            // update total units
            vars.idata.totalUnitsApproved = (
                uint256(vars.idata.totalUnitsApproved) +
                uint256(units) -
                uint256(vars.sdata.units)
            ).toUint128();
            token.updateAgreementData(iId, _encodeIndexData(vars.idata));
        } else if (vars.exist) {
            // if the subscription exists and approved, update the pending units amount

            // update pending subscription units of the publisher
            vars.idata.totalUnitsPending = (
                uint256(vars.idata.totalUnitsPending) +
                uint256(units) -
                uint256(vars.sdata.units)
            ).toUint128();
            token.updateAgreementData(iId, _encodeIndexData(vars.idata));
        } else {
            // if the E_NO_SUBS, create it and then update the pending units amount

            // create unallocated subscription
            vars.sdata = SubscriptionData({
                publisher: publisher,
                indexId: indexId,
                subId: _UNALLOCATED_SUB_ID,
                units: units,
                indexValue: vars.idata.indexValue
            });
            token.createAgreement(sId, _encodeSubscriptionData(vars.sdata));

            vars.idata.totalUnitsPending = vars.idata.totalUnitsPending.add(units, "IDA: E_OVERFLOW");
            token.updateAgreementData(iId, _encodeIndexData(vars.idata));
        }

        int256 balanceDelta = int256(vars.idata.indexValue - vars.sdata.indexValue) * int256(vars.sdata.units);

        // adjust publisher's deposit and balances if subscription is pending
        if (vars.sdata.subId == _UNALLOCATED_SUB_ID) {
            _adjustPublisherDeposit(token, publisher, -balanceDelta);
            token.settleBalance(publisher, -balanceDelta);
        }

        // settle subscriber static balance
        token.settleBalance(subscriber, balanceDelta);

        // update subscription data if necessary
        if (vars.exist) {
            vars.sdata.indexValue = vars.idata.indexValue;
            vars.sdata.units = units;
            token.updateAgreementData(sId, _encodeSubscriptionData(vars.sdata));
        }

        // check account solvency
        require(!token.isAccountCriticalNow(publisher), "IDA: E_LOW_BALANCE");

        // after-hook callback
        if (vars.exist) {
            vars.cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP;
            AgreementLibrary.callAppAfterCallback(vars.cbStates, vars.cbdata, newCtx);
        } else {
            vars.cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP;
            AgreementLibrary.callAppAfterCallback(vars.cbStates, vars.cbdata, newCtx);
        }

        emit IndexUnitsUpdated(token, publisher, indexId, subscriber, units, context.userData);
        emit SubscriptionUnitsUpdated(token, subscriber, publisher, indexId, units, context.userData);
    }

    /// @dev IInstantDistributionAgreementV1.getSubscription implementation
    function getSubscription(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber
    )
        external view override
        returns (
            bool exist,
            bool approved,
            uint128 units,
            uint256 pendingDistribution
        )
    {
        _StackVars memory vars;
        vars.iId = _getPublisherId(publisher, indexId);
        (vars.exist, vars.idata) = _getIndexData(token, vars.iId);
        require(vars.exist, "IDA: E_NO_INDEX");
        vars.sId = _getSubscriptionId(subscriber, vars.iId);
        (vars.exist, vars.sdata) = _getSubscriptionData(token, vars.sId);
        if (!vars.exist) return (false, false, 0, 0);
        assert(vars.sdata.publisher == publisher);
        assert(vars.sdata.indexId == indexId);
        exist = true;
        approved = vars.sdata.subId != _UNALLOCATED_SUB_ID;
        units = vars.sdata.units;
        pendingDistribution = approved ? 0 :
            uint256(vars.idata.indexValue - vars.sdata.indexValue) * uint256(vars.sdata.units);
    }

    /// @dev IInstantDistributionAgreementV1.getSubscriptionByID implementation
    function getSubscriptionByID(
       ISuperfluidToken token,
       bytes32 agreementId
    )
       external view override
       returns(
           address publisher,
           uint32 indexId,
           bool approved,
           uint128 units,
           uint256 pendingDistribution
       )
    {
        _StackVars memory vars;
        (vars.exist, vars.sdata) = _getSubscriptionData(token, agreementId);
        require(vars.exist, "IDA: E_NO_SUBS");

        publisher = vars.sdata.publisher;
        indexId = vars.sdata.indexId;
        vars.iId = _getPublisherId(publisher, indexId);
        (vars.exist, vars.idata) = _getIndexData(token, vars.iId);
        require(vars.exist, "IDA: E_NO_INDEX");

        approved = vars.sdata.subId != _UNALLOCATED_SUB_ID;
        units = vars.sdata.units;
        pendingDistribution = approved ? 0 :
            uint256(vars.idata.indexValue - vars.sdata.indexValue) * uint256(vars.sdata.units);
    }

    /// @dev IInstantDistributionAgreementV1.listSubscriptions implementation
    function listSubscriptions(
        ISuperfluidToken token,
        address subscriber
    )
        external view override
        returns(
            address[] memory publishers,
            uint32[] memory indexIds,
            uint128[] memory unitsList)
    {
        bytes32[] memory sidList = _listSubscriptionIds(token, subscriber);
        bool exist;
        SubscriptionData memory sdata;
        publishers = new address[](sidList.length);
        indexIds = new uint32[](sidList.length);
        unitsList = new uint128[](sidList.length);
        for (uint32 subId = 0; subId < sidList.length; ++subId) {
            bytes32 sId = sidList[subId];
            (exist, sdata) = _getSubscriptionData(token, sId);
            require(exist, "IDA: E_NO_SUBS");
            assert(sdata.subId == subId);
            publishers[subId] = sdata.publisher;
            indexIds[subId] = sdata.indexId;
            unitsList[subId] = sdata.units;
        }
    }

    /// @dev IInstantDistributionAgreementV1.deleteSubscription implementation
    function deleteSubscription(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes calldata ctx
    )
        external override
        returns(bytes memory newCtx)
    {
        _StackVars memory vars;
        ISuperfluid.Context memory context = AgreementLibrary.authorizeTokenAccess(token, ctx);
        address sender = context.msgSender;
        require(sender == publisher || sender == subscriber, "IDA: E_NOT_ALLOWED");
        vars.iId = _getPublisherId(publisher, indexId);
        vars.sId = _getSubscriptionId(subscriber, vars.iId);
        (vars.exist, vars.idata) = _getIndexData(token, vars.iId);
        require(vars.exist, "IDA: E_NO_INDEX");
        (vars.exist, vars.sdata) = _getSubscriptionData(token, vars.sId);
        require(vars.exist, "IDA: E_NO_SUBS");
        assert(vars.sdata.publisher == publisher);
        assert(vars.sdata.indexId == indexId);

        vars.cbStates = AgreementLibrary.createCallbackInputs(
            token,
            sender == subscriber ? publisher : subscriber,
            vars.sId,
            "");
        newCtx = ctx;

        vars.cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;
        vars.cbdata = AgreementLibrary.callAppBeforeCallback(vars.cbStates, newCtx);

        int256 balanceDelta = int256(vars.idata.indexValue - vars.sdata.indexValue) * int256(vars.sdata.units);

        // update publisher index agreement data
        if (vars.sdata.subId != _UNALLOCATED_SUB_ID) {
            vars.idata.totalUnitsApproved = vars.idata.totalUnitsApproved.sub(vars.sdata.units, "IDA: E_OVERFLOW");
        } else {
            vars.idata.totalUnitsPending = vars.idata.totalUnitsPending.sub(vars.sdata.units, "IDA: E_OVERFLOW");
        }
        token.updateAgreementData(vars.iId, _encodeIndexData(vars.idata));

        // terminate subscription agreement data
        token.terminateAgreement(vars.sId, 2);
        // remove subscription from subscriber's bitmap
        if (vars.sdata.subId != _UNALLOCATED_SUB_ID) {
            _clearSubsBitmap(token, subscriber, vars.sdata);
        }

        // move from publisher's deposit to static balance
        if (vars.sdata.subId == _UNALLOCATED_SUB_ID) {
            _adjustPublisherDeposit(token, publisher, -balanceDelta);
            token.settleBalance(publisher, -balanceDelta);
        }

        // settle subscriber static balance
        token.settleBalance(subscriber, balanceDelta);

        vars.cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;
        AgreementLibrary.callAppAfterCallback(vars.cbStates, vars.cbdata, newCtx);

        emit IndexUnsubscribed(token, publisher, indexId, subscriber, context.userData);
        emit SubscriptionDeleted(token, subscriber, publisher, indexId, context.userData);
    }

    function claim(
        ISuperfluidToken token,
        address publisher,
        uint32 indexId,
        address subscriber,
        bytes calldata ctx
    )
        external override
        returns(bytes memory newCtx)
    {
        _StackVars memory vars;
        vars.iId = _getPublisherId(publisher, indexId);
        vars.sId = _getSubscriptionId(subscriber, vars.iId);
        (vars.exist, vars.idata) = _getIndexData(token, vars.iId);
        require(vars.exist, "IDA: E_NO_INDEX");
        (vars.exist, vars.sdata) = _getSubscriptionData(token, vars.sId);
        require(vars.exist, "IDA: E_NO_SUBS");
         // sanity check
        assert(vars.sdata.publisher == publisher);
        assert(vars.sdata.indexId == indexId);
        // required condition check
        require(vars.sdata.subId == _UNALLOCATED_SUB_ID, "IDA: E_SUBS_APPROVED");

        uint256 pendingDistribution = uint256(vars.idata.indexValue - vars.sdata.indexValue)
            * uint256(vars.sdata.units);

        vars.cbStates = AgreementLibrary.createCallbackInputs(
            token,
            publisher,
            vars.sId,
            "");
        newCtx = ctx;

        if (pendingDistribution > 0) {
            vars.cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP;
            vars.cbdata = AgreementLibrary.callAppBeforeCallback(vars.cbStates, newCtx);

            // adjust publisher's deposits
            _adjustPublisherDeposit(token, publisher, -int256(pendingDistribution));
            token.settleBalance(publisher, -int256(pendingDistribution));

            // update subscription data and adjust subscriber's balance
            vars.sdata.indexValue = vars.idata.indexValue;
            token.updateAgreementData(vars.sId, _encodeSubscriptionData(vars.sdata));
            token.settleBalance(subscriber, int256(pendingDistribution));

            vars.cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP;
            AgreementLibrary.callAppAfterCallback(vars.cbStates, vars.cbdata, newCtx);
        } else {
            // nothing to be recorded in this case
            newCtx = ctx;
        }
    }

    /**************************************************************************
     * internal
     *************************************************************************/

    function _getPublisherId(
        address publisher,
        uint32 indexId)
        private pure
        returns (bytes32 iId)
    {
        return keccak256(abi.encodePacked("publisher", publisher, indexId));
    }

    function _getSubscriptionId(
        address subscriber,
        bytes32 iId)
        private pure
        returns (bytes32 sId)
    {
        return keccak256(abi.encodePacked("subscription", subscriber, iId));
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
        private pure
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
        ISuperfluidToken token,
        bytes32 iId)
        private view
        returns (bool exist)
    {
        bytes32[] memory adata = token.getAgreementData(address(this), iId, 2);
        uint256 a = uint256(adata[0]);
        exist = a > 0;
    }

    function _getIndexData(
        ISuperfluidToken token,
        bytes32 iId)
        private view
        returns (bool exist, IndexData memory idata)
    {
        bytes32[] memory adata = token.getAgreementData(address(this), iId, 2);
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
        ISuperfluidToken token,
        address publisher
    )
        private view
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
        ISuperfluidToken token,
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
        private pure
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
        ISuperfluidToken token,
        bytes32 sId)
        private view
        returns (bool exist, SubscriptionData memory sdata)
    {
        bytes32[] memory adata = token.getAgreementData(address(this), sId, 2);
        uint256 a = uint256(adata[0]);
        uint256 b = uint256(adata[1]);
        exist = a > 0;
        if (exist) {
            sdata.publisher = address(uint160(a >> (12*8)));
            sdata.indexId = uint32((a >> 32) & type(uint32).max);
            sdata.subId = uint32(a & type(uint32).max);
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
    // For each subscription approved it allocated with a subId with a value in [0, _MAX_NUM_SUBS).
    // The allocation is to fill one bit in the subscription bitmap.
    //
    // ## Subscription reference state slots
    //
    // slotId: _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START + subId)
    //
    // It stores the index data ID.

    function _findAndFillSubsBitmap(
        ISuperfluidToken token,
        address subscriber,
        bytes32 iId
    )
        private
        returns (uint32 subId)
    {
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
        ISuperfluidToken token,
        address subscriber,
        SubscriptionData memory sdata
    )
        private
    {
        uint256 subsBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            subscriber,
            _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID, 1)[0]);
        bytes32[] memory slotData = new bytes32[](1);
        slotData[0] = bytes32(subsBitmap & ~(1 << uint256(sdata.subId)));
        // zero the data
        token.updateAgreementStateSlot(
            subscriber,
            _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID,
            slotData);
    }

    function _listSubscriptionIds(
       ISuperfluidToken token,
       address subscriber
    )
       private view
       returns (bytes32[] memory sidList)
    {
       uint256 subsBitmap = uint256(token.getAgreementStateSlot(
           address(this),
           subscriber,
           _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID, 1)[0]);

       sidList = new bytes32[](_MAX_NUM_SUBS);
       // read all slots
       uint nSlots;
       for (uint32 subId = 0; subId < _MAX_NUM_SUBS; ++subId) {
           if ((uint256(subsBitmap >> subId) & 1) == 0) continue;
           bytes32 iId = token.getAgreementStateSlot(
               address(this),
               subscriber,
               _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START + subId, 1)[0];
           sidList[nSlots++] = _getSubscriptionId(subscriber, iId);
       }
       // resize memory arrays
       assembly {
           mstore(sidList, nSlots)
       }
    }

}
