// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperApp,
    SuperAppDefinitions
} from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";


library AgreementLibrary {

    struct Context {
        address msgSender;
        uint8 appLevel;
        int256 allowance;
        int256 allowanceUsed;
    }

    function decodeCtx(ISuperfluid host, bytes memory ctx)
        internal pure
        returns (Context memory context)
    {
        (
            context.msgSender,
            ,
            context.appLevel,
            context.allowance,
            context.allowanceUsed
        ) = host.decodeCtx(ctx);
    }

    function _needCallback(
        ISuperfluid host,
        address account,
        uint256 noopBit
    )
        private view
        returns (bool)
    {
        (bool isSuperApp, uint256 configWord) = host.getAppManifest(ISuperApp(account));
        return isSuperApp && ((configWord & noopBit) == 0);
    }

    function _beforeAgreement(
        bytes4 selector,
        uint256 noopBit,
        ISuperfluid host,
        ISuperfluidToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bool isTermination
    )
        private
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        if (_needCallback(host, account, noopBit)) {
            bytes memory data = abi.encodeWithSelector(
                selector,
                token,
                ctx,
                agreementClass,
                agreementId
            );
            (cbdata, newCtx) = host.callAppBeforeCallback(ISuperApp(account), data, isTermination, ctx);
        } else {
            newCtx = ctx;
        }
    }

    function _afterAgreement(
        bytes4 selector,
        uint256 noopBit,
        ISuperfluid host,
        ISuperfluidToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata,
        int256 appAllowance,
        bool isTermination
    )
        private
        returns(Context memory context, bytes memory newCtx)
    {
        if (_needCallback(host, account, noopBit)) {
            newCtx = ISuperfluid(msg.sender).ctxUpdateAllowance(ctx, appAllowance);

            bytes memory data = abi.encodeWithSelector(
                selector,
                token,
                newCtx,
                agreementClass,
                agreementId,
                cbdata
            );
            newCtx = host.callAppAfterCallback(ISuperApp(account), data, isTermination, ctx);
            context = AgreementLibrary.decodeCtx(ISuperfluid(msg.sender), newCtx);

            // sanity check of allowanceUsed return value
            if (appAllowance > 0) {
                // app allowance can be used by the app
                // agreement must not refund allowance if not requested (allowance < 0)
                assert(context.allowanceUsed >= 0);
                // agreement must only use up to allowance given
                assert(context.allowanceUsed <= appAllowance);
                // pay for app allowance
            } else if (appAllowance < 0) {
                // app allowance must be refunded
                assert(context.allowanceUsed == appAllowance);
            } // trivial casae no action
        } else {
            newCtx = ctx;
        }
    }

    function beforeAgreementCreated(
        ISuperfluid host,
        ISuperfluidToken token,
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
        ISuperfluidToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata,
        int256 appAllowance
    )
        internal
        returns(Context memory context, bytes memory newCtx)
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
            appAllowance,
            false
        );
    }

    function beforeAgreementUpdated(
        ISuperfluid host,
        ISuperfluidToken token,
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
        ISuperfluidToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata,
        int256 appAllowance
    )
        internal
        returns(Context memory context, bytes memory newCtx)
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
            appAllowance,
            false
        );
    }

    function beforeAgreementTerminated(
        ISuperfluid host,
        ISuperfluidToken token,
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
        ISuperfluidToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata,
        int256 appAllowance
    )
        internal
        returns(Context memory context, bytes memory newCtx)
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
            appAllowance,
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
