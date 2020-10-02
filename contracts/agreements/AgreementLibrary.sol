// SPDX-License-Identifier: MIT
pragma solidity 0.7.1;

import "../interfaces/superfluid/ISuperfluid.sol";
import "../interfaces/superfluid/ISuperApp.sol";
import "../interfaces/superfluid/SuperAppDefinitions.sol";

library AgreementLibrary {

    struct Context {
        uint8 appLevel;
        address msgSender;
        uint256 allowance;
        uint256 allowanceUsed;
    }

    function updateCtx(ISuperfluid host, bytes memory ctx, Context memory context)
        internal
        returns (bytes memory newCtx)
    {
        newCtx = host.ctxUpdate(
            ctx,
            context.appLevel,
            context.allowance,
            context.allowanceUsed);
    }

    function decodeCtx(ISuperfluid host, bytes memory ctx)
        internal
        pure
        returns (Context memory context)
    {
        (
            ,
            context.appLevel,
            context.msgSender,
            context.allowance,
            context.allowanceUsed
        ) = host.decodeCtx(ctx);
    }

    function _beforeAgreement(
        bytes4 selector,
        uint256 noopBit,
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bool isTermination
    )
        private
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        newCtx = ctx;
        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(ISuperApp(account));

        if (isSuperApp &&
            ((configWord & noopBit) == 0)) {
            bytes memory data = abi.encodeWithSelector(
                selector,
                token,
                ctx,
                agreementClass,
                agreementId
            );
            (cbdata, newCtx) = host.callAppBeforeCallback(ISuperApp(account), data, isTermination, ctx);
        }
    }

    function _afterAgreement(
        bytes4 selector,
        uint256 noopBit,
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata,
        bool isTermination
    )
        private
        returns(bytes memory newCtx)
    {
        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(ISuperApp(account));


        if (isSuperApp &&
            ((configWord & noopBit) == 0)) {
            bytes memory data = abi.encodeWithSelector(
                selector,
                token,
                ctx,
                agreementClass,
                agreementId,
                cbdata
            );
            return host.callAppAfterCallback(ISuperApp(account), data, isTermination, ctx);
        }

        return ctx;
    }

    function beforeAgreementCreated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        return _beforeAgreement(
            ISuperApp.beforeAgreementCreated.selector,
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            false
        );
    }

    function afterAgreementCreated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata
    )
        internal
        returns(bytes memory newCtx)
    {

        return _afterAgreement(
            ISuperApp.afterAgreementCreated.selector,
            SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            cbdata,
            false
        );
    }

    function beforeAgreementUpdated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        return _beforeAgreement(
            ISuperApp.beforeAgreementUpdated.selector,
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            false
        );
    }

    function afterAgreementUpdated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata
    )
        internal
        returns(bytes memory newCtx)
    {
        return _afterAgreement(
            ISuperApp.afterAgreementUpdated.selector,
            SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            cbdata,
            false
        );
    }

    function beforeAgreementTerminated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        return _beforeAgreement(
            ISuperApp.beforeAgreementTerminated.selector,
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            true
        );
    }

    function afterAgreementTerminated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata
    )
        internal
        returns(bytes memory newCtx)
    {
        return _afterAgreement(
            ISuperApp.afterAgreementTerminated.selector,
            SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            cbdata,
            true
        );
    }

    function getGovernance()
        internal
        view
        returns(ISuperfluidGovernance gov)
    {
        return ISuperfluidGovernance(ISuperfluid(msg.sender).getGovernance());
    }
}
