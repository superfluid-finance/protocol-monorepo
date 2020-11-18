// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import {
    ISuperfluidGovernance,
    ISuperfluid,
    ISuperfluidToken,
    ISuperApp,
    SuperAppDefinitions
} from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";

import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/SafeCast.sol";


/**
 * @dev Helper library for building super agreement
 */
library AgreementLibrary {

    using SignedSafeMath for int256;
    using SafeCast for uint256;
    using SafeCast for int256;

    /**************************************************************************
     * Context helpers
     *************************************************************************/

    struct Context {
        address msgSender;
        uint8 appLevel;
        uint256 appAllowance;
        int256 appAllowanceUsed;
    }

    function decodeCtx(ISuperfluid host, bytes memory ctx)
        internal pure
        returns (Context memory context)
    {
        (
            context.msgSender,
            ,
            context.appLevel,
            context.appAllowance,
            context.appAllowanceUsed
        ) = host.decodeCtx(ctx);
    }

    /**************************************************************************
     * Agreement callback helpers
     *************************************************************************/

    // TODO let user call it instead
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

    struct CallbackInputs {
        ISuperfluid host;
        ISuperfluidToken token;
        uint256 noopBit;
        address agreementClass;
        address account;
        bytes32 agreementId;
        uint256 appAllowance;
        int256 appAllowanceUsed;
    }

    function _adjustAppAllowanceUsed(
        uint256 currentAppAllowance,
        int256 newAppAllowanceUsed,
        bytes memory ctx
    )
        private
        returns (bool adjusted, bytes memory newCtx)
    {
        int256 signedCurrentAppAllowance = currentAppAllowance.toInt256();
        // app allowance used range: [0, currentAppAllowance]
        int256 adjustedNewAppAllowanceUsed = newAppAllowanceUsed > 0 ? (
            newAppAllowanceUsed > signedCurrentAppAllowance ?
            currentAppAllowance.toInt256() : newAppAllowanceUsed
        ) : 0;

        if (adjustedNewAppAllowanceUsed != newAppAllowanceUsed) {
            return (true, ISuperfluid(msg.sender).ctxUpdateAppAllowance(
                ctx,
                currentAppAllowance,
                adjustedNewAppAllowanceUsed
            ));
        } else {
            return (false, ctx);
        }
    }

    function callAppBeforeCallback(
        CallbackInputs memory inputs,
        bytes memory ctx
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        bytes4 selector;
        if (inputs.noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP) {
            selector = ISuperApp.beforeAgreementCreated.selector;
        } else if (inputs.noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP) {
            selector = ISuperApp.beforeAgreementUpdated.selector;
        } else {
            assert(inputs.noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP);
            selector = ISuperApp.beforeAgreementTerminated.selector;
        }

        if (_needCallback(inputs.host, inputs.account, inputs.noopBit)) {
            (cbdata, newCtx) = inputs.host.callAppBeforeCallback(
                ISuperApp(inputs.account),
                abi.encodeWithSelector(
                    selector,
                    inputs.token,
                    ctx,
                    inputs.agreementClass,
                    inputs.agreementId
                ),
                inputs.noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP,
                ctx);
        } else {
            newCtx = ctx;
        }
    }

    function callAppAfterCallback(
        CallbackInputs memory inputs,
        bytes memory cbdata,
        bytes memory ctx
    )
        internal
        returns(Context memory appContext, bytes memory newCtx)
    {
        bytes4 selector;
        if (inputs.noopBit == SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP) {
            selector = ISuperApp.afterAgreementCreated.selector;
        } else if (inputs.noopBit == SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP) {
            selector = ISuperApp.afterAgreementUpdated.selector;
        } else {
            assert(inputs.noopBit == SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP);
            selector = ISuperApp.afterAgreementTerminated.selector;
        }

        if (_needCallback(inputs.host, inputs.account, inputs.noopBit)) {
            Context memory currentContext = decodeCtx(ISuperfluid(msg.sender), ctx);

            // app allowance params stack PUSH
            // pass app allowance for the app (positive as loans, negative as refund request)
            newCtx = ISuperfluid(msg.sender).ctxUpdateAppAllowance(
                ctx,
                inputs.appAllowance,
                inputs.appAllowanceUsed
            );

            newCtx = inputs.host.callAppAfterCallback(
                ISuperApp(inputs.account),
                abi.encodeWithSelector(
                    selector,
                    inputs.token,
                    newCtx,
                    inputs.agreementClass,
                    inputs.agreementId,
                    cbdata
                ),
                inputs.noopBit == SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP,
                newCtx);

            appContext = decodeCtx(ISuperfluid(msg.sender), newCtx);
            bool adjusted;
            (adjusted, newCtx) = _adjustAppAllowanceUsed(
                inputs.appAllowance,
                appContext.appAllowanceUsed,
                newCtx
            );
            if (adjusted) {
                appContext = decodeCtx(ISuperfluid(msg.sender), newCtx);
            }

            // app allowance params stack POP
            newCtx = ISuperfluid(msg.sender).ctxUpdateAppAllowance(
                ctx,
                currentContext.appAllowance,
                appContext.appAllowanceUsed
            );
        } else {
            newCtx = ctx;
        }
    }

    /**************************************************************************
     * Misc
     *************************************************************************/

    // TODO move to signed math utils
    //function max(int256 a, int256 b) internal pure returns (int256) { return a > b ? a : b; }

    // function min(int256 a, int256 b) internal pure returns (int256) { return a > b ? b : a; }
}
