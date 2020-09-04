// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { IInstantDistributionAgreementV1, ISuperToken } from "../interfaces/IInstantDistributionAgreementV1.sol";
import { ISuperfluid } from "../interfaces/ISuperfluid.sol";
import { ContextLibrary } from "../superfluid/ContextLibrary.sol";

contract InstantDistributionAgreementV1 is IInstantDistributionAgreementV1 {

    uint32 public constant N_SLOTS = 256;

    struct PublisherData {
        uint128 indexValue;
        uint128 totalUnits;
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
        bytes calldata state_,
        uint256 /*time*/
    )
        external
        view
        override
        returns (int256 amount) {
        bool exist;
        bytes memory state = state_; // copy to memory for assembly
        PublisherData memory pdata;
        SubscriptionData memory sdata;
        if (state.length == 0) return 0;
        // load slot map
        uint256 slotBitmap;
        assembly {
            slotBitmap := mload(add(state, 0x20))
        }
        bytes32 pId;
        // read all slots
        for (uint32 slotId = 0; slotId < N_SLOTS; ++slotId) {
            if ((uint256(slotBitmap >> slotId) & 1) == 0) continue;
            assembly {
                pId := mload(add(state, mul(0x20, add(slotId , 2))))
            }
            bytes32 sId = _getSubscriptionId(subscriber, pId);
            (exist, pdata) = _getPublisherData(token, pId);
            require(exist, "IDAv1: index does not exist");
            (exist, sdata) = _getSubscriptionData(token, sId);
            require(exist, "IDAv1: subscription does not exist");
            amount += int256(pdata.indexValue - sdata.indexValue) * int256(sdata.units);
        }
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
        require(token.getAgreementData(address(this), pId).length == 0, "IDAv1: index already exists");
        token.createAgreement(pId, _encodePublisherData(PublisherData(0, 0)));
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
        pdata.indexValue = indexValue;
        token.updateAgreementData(pId, _encodePublisherData(pdata));
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
        totalUnits = pdata.totalUnits;
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
        require(token.getAgreementData(address(this), sId).length == 0, "IDAv1: subscription already exists");
        // add to subscription list of the subscriber
        SubscriptionData memory sdata = SubscriptionData({
            publisher: publisher,
            indexId: indexId,
            slotId: slotId,
            units: 0,
            indexValue: pdata.indexValue
        });
        token.createAgreement(sId, _encodeSubscriptionData(sdata));
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
        require(exist, "IDAv1: subscription does not exist");
        // update total units
        if (units > sdata.units) {
            pdata.totalUnits += units - sdata.units; // FIXME safe128
        } else {
            pdata.totalUnits -= units - sdata.units; // FIXME safe128
        }
        token.updateAgreementData(pId, _encodePublisherData(pdata));
        // update subscriptiond data
        // FIXME touch subscriber balance first
        //token.settle(subscriber, delta);
        sdata.indexValue = pdata.indexValue;
        sdata.units = units;
        token.updateAgreementData(sId, _encodeSubscriptionData(sdata));
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
                uint32[] memory indexIds) {
        bytes memory state = token.getAgreementAccountState(address(this), subscriber);
        bool exist;
        SubscriptionData memory sdata;
        if (state.length > 0) {
            // load slot map
            uint256 slotBitmap;
            assembly {
                slotBitmap := mload(add(state, 0x20))
            }
            bytes32 pId;
            // read all slots
            publishers = new address[](256);
            indexIds = new uint32[](256);
            for (uint32 slotId = 0; slotId < N_SLOTS; ++slotId) {
                if ((uint256(slotBitmap >> slotId) & 1) == 0) continue;
                assembly {
                    pId := mload(add(state, mul(0x20, add(slotId , 2))))
                }
                publishers[slotId] = address(uint240(uint256(pId)));
                bytes32 sId = _getSubscriptionId(subscriber, pId);
                (exist, sdata) = _getSubscriptionData(token, sId);
                require(exist, "IDAv1: subscription does not exist");
                // FIXME compress the list
                publishers[slotId] = sdata.publisher;
                indexIds[slotId] = sdata.indexId;
            }
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
        returns (bytes memory data) {
        return abi.encode(
            uint256(pdata.indexValue) |
            (uint256(pdata.totalUnits) << 128)
        );
    }

    function _getPublisherData(
        ISuperToken token,
        bytes32 pId)
        private
        view
        returns (bool exist, PublisherData memory pdata)
    {
        bytes memory adata = token.getAgreementData(address(this), pId);
        exist = adata.length > 0;
        if (exist) {
            uint256 v = abi.decode(adata, (uint256));
            pdata.indexValue = uint128(v & uint256(int128(-1)));
            pdata.totalUnits = uint128(v >> 128);
        }
    }

    function _encodeSubscriptionData(
        SubscriptionData memory sdata)
        private
        pure
        returns (bytes memory data) {
        return abi.encode(
            (uint256(sdata.publisher) << (12*8)) |
            (uint256(sdata.indexId)) << 32 |
            uint256(sdata.slotId),
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
        bytes memory adata = token.getAgreementData(address(this), sId);
        exist = adata.length > 0;
        if (exist) {
            (uint256 a, uint256 b) = abi.decode(adata, (uint256, uint256));
            sdata.publisher = address(uint160(a >> (12*8)));
            sdata.indexId = uint32((a & 0xFFFFFFFF) >> 32);
            sdata.slotId = uint32(a & 0xFFFF);
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
        bytes memory state = token.getAgreementAccountState(address(this), subscriber);
        if (state.length == 0) {
            // create new slot map
            // 1 word: slotBitmap
            // 256 words: slots storing pId array
            state = new bytes((N_SLOTS + 1) * 32);
            assembly {
                mstore(add(state, 0x20), 1)
                mstore(add(state, 0x40), pId)
            }
            token.updateAgreementAccountState(subscriber, state);
        } else {
            // load slot map
            uint256 slotBitmap;
            assembly {
                slotBitmap := mload(add(state, 0x20))
            }
            for (slotId = 0; slotId < N_SLOTS; ++slotId) {
                if ((uint256(slotBitmap >> slotId) & 1) == 0) {
                    // slot is empty
                    assembly {
                        mstore(state, or(slotBitmap, shl(1, slotId)))
                        mstore(add(state, mul(0x20, add(slotId , 2))), pId)
                    }
                    break;
                }
            }
        }
    }
}
