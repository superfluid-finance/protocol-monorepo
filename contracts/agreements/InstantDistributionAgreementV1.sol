// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { IInstantDistributionAgreementV1, ISuperToken } from "../interfaces/IInstantDistributionAgreementV1.sol";
import { ISuperfluid } from "../interfaces/ISuperfluid.sol";
import { ContextLibrary } from "../superfluid/ContextLibrary.sol";

contract InstantDistributionAgreementV1 is IInstantDistributionAgreementV1 {

    struct PublisherData {
        uint256 indexValue;
        uint256 totalUnits;
    }

    function agreementType() external override pure returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.IInstantDistributionAgreement.v1");
    }

    /// @notice Calculate the real-time balance using the state.
    /// @param state State to be used.
    /// @param time Future time used for the calculation.
    /// @return amount Account real-time balance.
    function realtimeBalanceOf(
        bytes calldata /*state*/,
        uint256 /*time*/
    )
        external
        pure
        override
        returns (int256 amount) {
        return 0;
    }

    /// @notice Change the timestamp of the state.
    /// @param state State to be used.
    /// @param time Time for the new state.
    /// @return newState New state.
    function touch(
        bytes calldata state,
        uint256 /*time*/
    )
        external
        pure
        override
        returns(bytes memory newState) {
        newState = state;
    }

    function createIndex(
        ISuperToken token,
        uint256 indexId,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        //ISuperfluid host = ISuperfluid(msg.sender);
        address publisher = ContextLibrary.decode(ctx).msgSender;
        bytes32 pId = _getPublisherId(publisher, indexId);
        bytes memory adata = token.getAgreementData(address(this), pId);
        require(adata.length == 0, "IDAv1: Index already exists");
        token.createAgreement(pId, _encodePublisherData(PublisherData(0, 0)));
    }

    function updateIndex(
        ISuperToken token,
        uint256 indexId,
        uint256 indexValue,
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

    function createSubscription(
        ISuperToken /*token*/,
        address /*receiver*/,
        uint256 /*units*/,
        bytes calldata ctx)
            external
            override
            returns(bytes memory newCtx) {
        // TODO
        newCtx = ctx;
    }

    function _getPublisherId(
        address publisher,
        uint256 indexId)
        private
        pure
        returns (bytes32 pId)
    {
        return keccak256(abi.encode("publisher", publisher, indexId));
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
            (pdata.indexValue, pdata.totalUnits) = abi.decode(adata, (uint256, uint256));
        }
    }

    function _encodePublisherData(
        PublisherData memory pdata)
        private
        pure
        returns (bytes memory data) {
        return abi.encode(pdata.indexValue, pdata.totalUnits);
    }
}
