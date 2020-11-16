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


/**
 * @dev Helper library for building super agreement
 */
library AgreementLibrary {

    using SignedSafeMath for int256;

    /**************************************************************************
     * Context helpers
     *************************************************************************/

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

    /**
     * @dev Calculate deltas required to satisfy the allowance used
     * @param currentAllowance allowance given to the app through the context by the allowance provider
     * @param currentAllowanceUsed allowance used so far by the app
     * @param newAllowanceUsed new allowance used during this call
     * @return accountAllowanceUsedDelta account allowance used delta to pay for the `newAllowanceUsed`
     * @return accountBalanceDelta account balance delta to self fund the missing amount in `newAllowanceUsed`
     *
     * NOTE:
     * - when `currentAllowance` is positive, the call can use up to that amount without self funding
     * - when `currentAllowance` is negative, the call need to refund the exact absolute amount
     */
    function applyAllowanceUsed(
        int256 currentAllowance,
        int256 currentAllowanceUsed,
        int256 newAllowanceUsed
    )
        internal pure
        returns (
            int256 accountAllowanceUsedDelta,
            int256 accountBalanceDelta
        )
    {
        // in fairness, the code for currentAllowance < 0 and > 0 are exactly same apart from the min/max logic
        // which can be merged into one usinb abs() probably, but keeping them separated in the code in order
        // to keep the nice comments and diagrams visible for this heavy logic

        if (currentAllowance > 0) {
            // up to this amount can be used as allowance

            assert(currentAllowanceUsed >= 0 && currentAllowanceUsed <= currentAllowance);
            // 0 -> positive
            // |--------- CA --------->|
            // |--- CAL -->|--- CAU -->|
            int256 currentAllowanceLeft = currentAllowance.sub(currentAllowanceUsed);

            if (newAllowanceUsed > 0) {
                // allowance being used

                // use up to the current context allowance amount
                if (currentAllowanceLeft < newAllowanceUsed) {
                    // 0 -> positive
                    // |--------- CA --------->|
                    // |--- CAL --->|-- CAU -->|
                    // |------- NAU ------>|
                    // |-- AUD -->|<- BD --|

                    // free up to the current context allowance amount
                    accountAllowanceUsedDelta = currentAllowanceLeft;
                    // rest paid by sender
                    accountBalanceDelta = currentAllowanceLeft.sub(newAllowanceUsed);
                } else {
                    // 0 -> positive
                    // |--------- CA --------->|
                    // |--- CAL --->|-- CAU -->|
                    // |- NAU ->|
                    // |- AUD ->|

                    // use up to what app used,
                    accountAllowanceUsedDelta = newAllowanceUsed;
                }
            } else {
                // newAllowanceUsed < 0
                // allowance being given back

                // 0 -> positive
                // |--------- CA --------->|
                // |--- CAL --->|-- CAU -->|
                //                |<- NAU -|
                //                |<- AUD -|
                //
                // OR
                //
                // 0 -> positive
                // |--------- CA --------->|
                // |---- CAL ---->|- CAU ->|
                //       |<----- NAU ------|
                //       |<- BD --|<- AUD -|

                // not more than the current allowance used
                accountAllowanceUsedDelta = max(newAllowanceUsed, currentAllowanceUsed.mul(-1));
                // refund account balance
                accountBalanceDelta =  newAllowanceUsed.sub(accountAllowanceUsedDelta);
            }
        } else if (currentAllowance < 0) {
            // allowance being refunded

            assert(currentAllowanceUsed <= 0 && currentAllowanceUsed >= currentAllowance);
            //             negative <- 0
            // |<-------- CA ----------|
            // |<-- CAL ---|<-- CAU ---|
            int256 currentAllowanceLeft = currentAllowance.sub(currentAllowanceUsed);

            if (newAllowanceUsed < 0) {
                // allowance being given back

                // use up to the current context allowance amount
                if (currentAllowanceLeft > newAllowanceUsed) {
                    //             negative <- 0
                    // |<-------- CA ----------|
                    // |<-------- CAU ---------|
                    // |<-- CAL ---|<-- CAU ---|
                    // |<------ NAU --------|
                    // |<-- AUD ---|-- BD ->|

                    // free up to the current context allowance amount
                    accountAllowanceUsedDelta = currentAllowanceLeft;
                    // rest refund to sender
                    accountBalanceDelta = currentAllowanceLeft.sub(newAllowanceUsed);
                } else {
                    //             negative <- 0
                    // |<-------- CA ----------|
                    // |<-------- CAU ---------|
                    // |<-- CAL ---|<-- CAU ---|
                    // |<- NAU --|
                    // |<- AUD --|

                    // refund to what is requested
                    accountAllowanceUsedDelta = newAllowanceUsed;
                }
            } else {
                // more allowance wanted
                // newAllowanceUsed > 0

                //             negative <- 0
                // |<-------- CA ----------|
                // |<-------- CAU ---------|
                // |<-- CAL ---|<-- CAU ---|
                //                |- NAU ->|
                //                |-- AUD >|
                //
                // OR
                //             negative <- 0
                // |<-------- CA ----------|
                // |<-------- CAU ---------|
                // |<-- CAL ---|<-- CAU ---|
                //      |------ NAU ------>|
                //      |- BD >|--- AUD -->|

                // not more than the current refund requested
                accountAllowanceUsedDelta = min(newAllowanceUsed, currentAllowanceUsed.mul(-1));
                // refund account balance
                accountBalanceDelta =  newAllowanceUsed.sub(accountAllowanceUsedDelta);
            }
        }
    }

    function applyAllowanceUsedAndUpdate(
        ISuperfluidToken token,
        address account,
        int256 currentAllowance,
        int256 currentAllowanceUsed,
        int256 newAllowanceUsed,
        bytes memory ctx
    )
        internal
        returns (bytes memory newCtx)
    {
        (int accountAllowanceUsedDelta, int accountBalanceDelta) = applyAllowanceUsed(
            currentAllowance,
            currentAllowanceUsed,
            newAllowanceUsed
        );

        if (accountAllowanceUsedDelta != 0) {
            newCtx = ISuperfluid(msg.sender).ctxUpdateAllowanceUsed(
                ctx,
                currentAllowanceUsed.add(accountAllowanceUsedDelta));
        } else {
            newCtx = ctx;
        }

        if (accountBalanceDelta != 0) {
            token.settleBalance(account, accountBalanceDelta);
        }
    }

    /**************************************************************************
     * Agreement callback helpers
     *************************************************************************/

    // TODO optimize this out
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
            // pass app allowance for the app (positive as loans, negative as refund request)
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

            // sanity checks of allowanceUsed returned, agreement implemntation shal never fail these asserts
            if (appAllowance > 0) {
                // app allowance can be used by the app
                // agreement must not refund allowance if not requested
                assert(context.allowanceUsed >= 0);
                // agreement must only use up to allowance given
                assert(context.allowanceUsed <= appAllowance);
                // pay for app allowance
            } else if (appAllowance <= 0) {
                // app allowance must be refunded
                // agreement must not use allowance if not given ()
                assert(context.allowanceUsed <= 0);
                // agreement must only refund up to allowance amount
                assert(context.allowanceUsed >= appAllowance);
                // app must pay for the discrepency
                if (context.allowanceUsed > appAllowance) {
                    token.settleBalance(account, appAllowance - context.allowanceUsed);
                    require(token.isAccountSolventNow(account), "SF: Account become insolvent after callback");
                }
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

    /**************************************************************************
     * Misc
     *************************************************************************/

    function getGovernance()
        internal view
        returns(ISuperfluidGovernance gov)
    {
        return ISuperfluidGovernance(ISuperfluid(msg.sender).getGovernance());
    }

    function max(int256 a, int256 b) internal pure returns (int256) { return a > b ? a : b; }

    function min(int256 a, int256 b) internal pure returns (int256) { return a > b ? b : a; }
}
