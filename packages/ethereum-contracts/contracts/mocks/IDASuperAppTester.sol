// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;
pragma abicoder v2;

import {
    ISuperfluid,
    ISuperToken,
    ISuperApp
} from "../superfluid/Superfluid.sol";
import {
    IInstantDistributionAgreementV1
} from "../interfaces/agreements/IInstantDistributionAgreementV1.sol";


contract IDASuperAppTester is ISuperApp {

    ISuperfluid private _host;
    IInstantDistributionAgreementV1 private _ida;
    ISuperToken private _token;
    uint32 private _indexId;

    constructor(
        ISuperfluid host,
        uint256 configWord,
        IInstantDistributionAgreementV1 ida,
        ISuperToken token,
        uint32 indexId)
    {
        _host = host;
        _host.registerApp(configWord);
        _ida = ida;
        _token = token;
        _indexId = indexId;

        _host.callAgreement(
            _ida,
            abi.encodeWithSelector(
                _ida.createIndex.selector,
                _token,
                _indexId,
                new bytes(0) // placeholder ctx
            ),
            new bytes(0) // user data
        );
    }

    function _expectCallback(
        bytes calldata ctx,
        bytes32 callbackType,
        ISuperToken superToken,
        address agreementClass,
        bytes calldata agreementData
    )
        private view
    {
        ISuperfluid.Context memory context = ISuperfluid(msg.sender).decodeCtx(ctx);
        bytes32 expectedCallbackType;
        address expectedSuperToken;
        address expectedAgreementClass;
        bytes memory expectedAgreementData;
        (
            expectedCallbackType,
            expectedSuperToken,
            expectedAgreementClass,
            expectedAgreementData
        ) = abi.decode(context.userData, (bytes32, address, address, bytes));
        require(expectedCallbackType == callbackType, "wrong callbackType");
        require(expectedSuperToken == address(superToken), "wrong superToken");
        require(expectedAgreementClass == agreementClass, "wrong agreementClass");
        require(keccak256(expectedAgreementData) == keccak256(agreementData), "wrong aAgreementData");
    }

    function beforeAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata agreementData,
        bytes calldata ctx
    )
        external view
        validCtx(ctx)
        virtual override
        returns (bytes memory /*cbdata*/)
    {
        _expectCallback(ctx, keccak256("create"), superToken, agreementClass, agreementData);
        return abi.encode(agreementId);
    }

    function afterAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata agreementData,
        bytes calldata cbdata,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        virtual override
        returns (bytes memory newCtx)
    {
        {
            bytes32 agreementId1 = abi.decode(cbdata, (bytes32));
            require(agreementId1 == agreementId, "agreementId changed");
        }
        _ida.getSubscriptionByID(superToken, agreementId);
        _expectCallback(ctx, keccak256("create"), superToken, agreementClass, agreementData);
        return ctx;
    }

    function beforeAgreementUpdated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata ctx
    )
        external view
        validCtx(ctx)
        virtual override
        returns (bytes memory /*cbdata*/)
    {
        return new bytes(0);
    }

    function afterAgreementUpdated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        virtual override
        returns (bytes memory newCtx)
    {
        return ctx;
    }

    function beforeAgreementTerminated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata ctx
    )
        external view
        validCtx(ctx)
        virtual override
        returns (bytes memory /*cbdata*/)
    {
        return new bytes(0);
    }

    function afterAgreementTerminated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        virtual override
        returns (bytes memory newCtx)
    {
        return ctx;
    }

    modifier validCtx(bytes calldata ctx) {
        require(ISuperfluid(msg.sender).isCtxValid(ctx), "IDASuperAppTester: ctx not valid before");
        _;
    }

}
