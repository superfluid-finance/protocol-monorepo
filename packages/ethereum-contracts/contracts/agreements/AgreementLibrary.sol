// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;
pragma experimental ABIEncoderV2;

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

    /**
     * @dev Authorize the msg.sender to access token agreement storage
     *
     * NOTE:
     * - msg.sender must be the expected host contract.
     * - it should revert on unauthorized access.
     */
    function authorizeTokenAccess(ISuperfluidToken token, bytes memory ctx)
        internal view
        returns (ISuperfluid.Context memory)
    {
        require(token.getHost() == msg.sender, "AgreementLibrary: unauthroized host");
        return ISuperfluid(msg.sender).decodeCtx(ctx);
    }

    /**************************************************************************
     * Agreement callback helpers
     *************************************************************************/

    struct CallbackInputs {
        uint256 noopMask;
        ISuperfluidToken token;
        address account;
        bytes32 agreementId;
        bytes agreementData;
        uint256 appAllowanceGranted;
        int256 appAllowanceUsed;
        uint256 noopBit;
    }

    function createCallbackInputs(
        ISuperfluidToken token,
        address account,
        bytes32 agreementId,
        bytes memory agreementData
    )
       internal view
       returns (CallbackInputs memory inputs)
    {
        ISuperfluid host = ISuperfluid(msg.sender);
        inputs.token = token;
        inputs.account = account;
        inputs.agreementId = agreementId;
        inputs.agreementData = agreementData;
        (bool isSuperApp, bool isJailed, uint256 noopMask) = host.getAppManifest(ISuperApp(account));
        // skip the callbacks if the app is already jailed
        inputs.noopMask = isSuperApp && !isJailed ? noopMask : type(uint256).max;
    }

    function callAppBeforeCallback(
        CallbackInputs memory inputs,
        bytes memory ctx
    )
        internal
        returns(bytes memory cbdata)
    {
        // this will check composit app whitelisting, do not skip!
        // otherwise an app could be trapped into an agreement
        bytes memory appCtx = _pushCallbackStack(ctx, inputs);
        if ((inputs.noopMask & inputs.noopBit) == 0) {
            bytes memory callData = abi.encodeWithSelector(
                _selectorFromNoopBit(inputs.noopBit),
                inputs.token,
                address(this) /* agreementClass */,
                inputs.agreementId,
                inputs.agreementData,
                new bytes(0) // placeholder ctx
            );
            cbdata = ISuperfluid(msg.sender).callAppBeforeCallback(
                ISuperApp(inputs.account),
                callData,
                inputs.noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP,
                appCtx);
        }
        _popCallbackStatck(ctx, 0);
    }

    function callAppAfterCallback(
        CallbackInputs memory inputs,
        bytes memory cbdata,
        bytes memory ctx
    )
        internal
        returns (ISuperfluid.Context memory appContext, bytes memory appCtx)
    {
        // this will check composit app whitelisting, do not skip!
        // otherwise an app could be trapped into an agreement
        appCtx = _pushCallbackStack(ctx, inputs);
        if ((inputs.noopMask & inputs.noopBit) == 0) {
            bytes memory callData = abi.encodeWithSelector(
                _selectorFromNoopBit(inputs.noopBit),
                inputs.token,
                address(this) /* agreementClass */,
                inputs.agreementId,
                inputs.agreementData,
                cbdata,
                new bytes(0) // placeholder ctx
            );
            appCtx = ISuperfluid(msg.sender).callAppAfterCallback(
                ISuperApp(inputs.account),
                callData,
                inputs.noopBit == SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP,
                appCtx);

            appContext = ISuperfluid(msg.sender).decodeCtx(appCtx);

            // adjust allowance used to the range [appAllowanceWanted..appAllowanceGranted]
            appContext.appAllowanceUsed = max(0, min(
                inputs.appAllowanceGranted.toInt256(),
                max(appContext.appAllowanceWanted.toInt256(), appContext.appAllowanceUsed)));

            appCtx = _popCallbackStatck(ctx, appContext.appAllowanceUsed);
        }
    }

    function _selectorFromNoopBit(uint256 noopBit)
        private pure
        returns (bytes4 selector)
    {
        if (noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP) {
            return ISuperApp.beforeAgreementCreated.selector;
        } else if (noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP) {
            return ISuperApp.beforeAgreementUpdated.selector;
        } else if (noopBit == SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP) {
            return ISuperApp.beforeAgreementTerminated.selector;
        } else if (noopBit == SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP) {
            return ISuperApp.afterAgreementCreated.selector;
        } else if (noopBit == SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP) {
            return ISuperApp.afterAgreementUpdated.selector;
        } else /* if (noopBit == SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP) */ {
            return ISuperApp.afterAgreementTerminated.selector;
        }
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
            ISuperApp(inputs.account),
            inputs.appAllowanceGranted,
            inputs.appAllowanceUsed);
    }

    function _popCallbackStatck(
        bytes memory ctx,
        int256 appAllowanceUsed
    )
        private
        returns (bytes memory appCtx)
    {
        // app allowance params stack POP
        return ISuperfluid(msg.sender).appCallbackPop(ctx, appAllowanceUsed);
    }

    /**************************************************************************
     * Misc
     *************************************************************************/

    function max(int256 a, int256 b) internal pure returns (int256) { return a > b ? a : b; }

    function min(int256 a, int256 b) internal pure returns (int256) { return a > b ? b : a; }
}
