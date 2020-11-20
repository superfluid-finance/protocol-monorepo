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
        uint256 timestamp;
        address msgSender;
        uint8 appLevel;
        int256 appAllowanceIO;
        int256 appAllowanceUsed;
    }

    function decodeCtx(ISuperfluid host, bytes memory ctx)
        internal pure
        returns (Context memory context)
    {
        (
            context.timestamp,
            context.msgSender,
            ,
            context.appLevel,
            context.appAllowanceIO,
            context.appAllowanceUsed
        ) = host.decodeCtx(ctx);
    }

    function ctxUpdateAppAllowance(
        bytes memory ctx,
        AgreementLibrary.Context memory currentContext,
        int256 appAllowanceWantedDelta,
        int256 appAllowanceUsedDelta
    )
        internal
        returns (bytes memory newCtx)
    {
        int256 appAllowanceWanted;
        // app allowance given (input in positive) / wanted (output in negative)
        if (currentContext.appAllowanceIO > 0) appAllowanceWanted = 0;
        else appAllowanceWanted = currentContext.appAllowanceIO;
        appAllowanceWanted = appAllowanceWanted.sub(appAllowanceWantedDelta);

        return ISuperfluid(msg.sender).ctxUpdateAppAllowance(
            ctx,
            currentContext.appLevel,
            // use negative as the output to the upper stack
            appAllowanceWanted,
            currentContext.appAllowanceUsed.add(appAllowanceUsedDelta)
        );
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
        uint256 appAllowance;
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
            Context memory currentContext = decodeCtx(ISuperfluid(msg.sender), ctx);
            Context memory appContext;

            // app allowance params stack PUSH
            // pass app allowance and current allowance used to the app,
            bytes memory appCtx = ISuperfluid(msg.sender).ctxUpdateAppAllowance(
                ctx,
                currentContext.appLevel + 1,
                inputs.appAllowance.toInt256(),
                inputs.appAllowanceUsed
            );

            (cbdata, appCtx) = ISuperfluid(msg.sender).callAppBeforeCallback(
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

            appContext = adjustNewAppAllowanceUsed(
                inputs.appAllowance,
                appCtx);
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
            Context memory currentContext = decodeCtx(ISuperfluid(msg.sender), ctx);

            // app allowance params stack PUSH
            // pass app allowance and current allowance used to the app,
            bytes memory appCtx = ISuperfluid(msg.sender).ctxUpdateAppAllowance(
                ctx,
                currentContext.appLevel + 1,
                inputs.appAllowance.toInt256(),
                inputs.appAllowanceUsed
            );

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

            appContext = adjustNewAppAllowanceUsed(
                inputs.appAllowance,
                appCtx);

            // app allowance params stack POP
            ISuperfluid(msg.sender).ctxUpdateAppAllowance(
                ctx,
                currentContext.appLevel,
                currentContext.appAllowanceIO,
                appContext.appAllowanceUsed
            );
        }
    }

    function adjustNewAppAllowanceUsed(
        uint256 appAllowance,
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
        return max(0, min(appAllowance.toInt256(), max(appAllowanceWanted, newAppAllowanceUsed)));
    }

    function adjustNewAppAllowanceUsed(
        uint256 appAllowance,
        bytes memory appCtx
    )
        internal view
        returns (Context memory appContext)
    {
        appContext = decodeCtx(ISuperfluid(msg.sender), appCtx);

        appContext.appAllowanceUsed = adjustNewAppAllowanceUsed(
            appAllowance,
            appContext.appAllowanceIO,
            appContext.appAllowanceUsed);
    }

    /**************************************************************************
     * Misc
     *************************************************************************/

    function max(int256 a, int256 b) internal pure returns (int256) { return a > b ? a : b; }

    function min(int256 a, int256 b) internal pure returns (int256) { return a > b ? b : a; }
}
