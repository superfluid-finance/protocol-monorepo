// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import {
    ISuperfluidGovernance,
    ISuperfluid,
    ISuperfluidToken,
    ISuperApp,
    SuperAppDefinitions,
    ContextDefinitions
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
        uint256 timestamp;
        address msgSender;
        uint8 appLevel;
        uint8 callType;
        int256 appAllowanceIO;
        int256 appAllowanceUsed;
    }

    function decodeCtx(ISuperfluid host, bytes memory ctx)
        internal pure
        returns (Context memory context)
    {
        uint256 callInfo;
        (
            callInfo,
            context.timestamp,
            context.msgSender,
            ,
            context.appAllowanceIO,
            context.appAllowanceUsed
        ) = host.decodeCtx(ctx);
        (context.appLevel, context.callType) = ContextDefinitions.decodeCallInfo(callInfo);
    }

    /**************************************************************************
     * Agreement callback helpers
     *************************************************************************/

    struct CallbackInputs {
        uint256 noopMask;
        address agreementClass;
        ISuperfluidToken token;
        address account;
        bytes32 agreementId;
        uint256 appAllowanceGranted;
        int256 appAllowanceUsed;
        uint256 noopBit;
        bytes4 selector;
    }

    function createCallbackInputs(
        address agreementClass,
        ISuperfluidToken token,
        address account,
        bytes32 agreementId
    )
       internal view
       returns (CallbackInputs memory inputs)
    {
        ISuperfluid host = ISuperfluid(msg.sender);
        (bool isSuperApp, bool isJailed, uint256 noopMask) = host.getAppManifest(ISuperApp(account));
        inputs.noopMask = isSuperApp && !isJailed ? noopMask : type(uint256).max;
        inputs.agreementClass = agreementClass;
        inputs.token = token;
        inputs.account = account;
        inputs.agreementId = agreementId;
    }

    function callAppBeforeCallback(
        CallbackInputs memory inputs,
        bytes memory ctx
    )
        internal
        returns(bytes memory cbdata)
    {
        if ((inputs.noopMask & inputs.noopBit) == 0) {
            bytes memory appCtx = _pushCallbackStack(ctx, inputs);

            (cbdata,) = ISuperfluid(msg.sender).callAppBeforeCallback(
                ISuperApp(inputs.account),
                abi.encodeWithSelector(
                    inputs.selector,
                    inputs.token,
                    appCtx,
                    inputs.agreementClass,
                    inputs.agreementId
                ),
                inputs.noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP,
                appCtx);

            _popCallbackStatck(ctx, 0);
        }
    }

    function callAppAfterCallback(
        CallbackInputs memory inputs,
        bytes memory cbdata,
        bytes memory ctx
    )
        internal
        returns(Context memory appContext)
    {
        if ((inputs.noopMask & inputs.noopBit) == 0) {
            bytes memory appCtx = _pushCallbackStack(ctx, inputs);

            appCtx = ISuperfluid(msg.sender).callAppAfterCallback(
                ISuperApp(inputs.account),
                abi.encodeWithSelector(
                    inputs.selector,
                    inputs.token,
                    appCtx,
                    inputs.agreementClass,
                    inputs.agreementId,
                    cbdata
                ),
                inputs.noopBit == SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP,
                appCtx);

            appContext = _adjustNewAppAllowanceUsed(
                inputs.appAllowanceGranted,
                appCtx);

            _popCallbackStatck(ctx, appContext.appAllowanceUsed);
        }
    }

    function calculateAdjustedNewAppAllowanceUsed(
        uint256 appAllowanceGranted,
        int256 appAllowanceWanted,
        int256 newAppAllowanceUsed
    )
        internal pure
        returns (int256)
    {
        // If allowance is used by agreements, app will set appAllowanceIO to a negative value
        // as app allowance wanted.
        // Otherwise when no agreement is used, app allowance wanted is zero.
        if (appAllowanceWanted > 0) return 0;
        else appAllowanceWanted = -appAllowanceWanted;
        // rules:
        // - give allowance up to app allowance
        // - refund until no less than app allowance wantedd
        return max(0, min(appAllowanceGranted.toInt256(), max(appAllowanceWanted, newAppAllowanceUsed)));
    }

    function _pushCallbackStack(
        bytes memory ctx,
        CallbackInputs memory inputs
    )
        private
        returns (bytes memory appCtx)
    {
        // app allowance params stack PUSH
        // pass app allowance and current allowance used to the app,
        appCtx = ISuperfluid(msg.sender).appCallbackPush(
            ctx,
            inputs.appAllowanceGranted,
            inputs.appAllowanceUsed);
    }

    function _popCallbackStatck(
        bytes memory ctx,
        int256 appAllowanceUsed
    )
        private
    {
        // app allowance params stack POP
        ISuperfluid(msg.sender).appCallbackPop(ctx, appAllowanceUsed);
    }

    // TODO move to superfluid
    function _adjustNewAppAllowanceUsed(
        uint256 appAllowanceGranted,
        bytes memory appCtx
    )
        private view
        returns (Context memory appContext)
    {
        appContext = decodeCtx(ISuperfluid(msg.sender), appCtx);

        appContext.appAllowanceUsed = calculateAdjustedNewAppAllowanceUsed(
            appAllowanceGranted,
            appContext.appAllowanceIO,
            appContext.appAllowanceUsed);
    }

    /**************************************************************************
     * Misc
     *************************************************************************/

    function max(int256 a, int256 b) internal pure returns (int256) { return a > b ? a : b; }

    function min(int256 a, int256 b) internal pure returns (int256) { return a > b ? b : a; }
}
