// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;

import "./ISuperfluid.sol";
import "./ISuperApp.sol";
import "./AppHelper.sol";

library AgreementLibrary {

    function beforeAgreementCreated(
        ISuperfluid host,
        bytes memory ctx,
        address receiver,
        bytes memory data
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(receiver);
        if (isSuperApp &&
            (configWord & AppHelper.BEFORE_AGREEMENT_CREATED_NOOP) != 0) {
            return host.callAppBefore(ctx, receiver, ISuperApp(receiver).beforeAgreementUpdated.selector, data);
        }

        return ("", "");
    }

    function afterAgreementCreated(
        ISuperfluid host,
        bytes memory ctx,
        address receiver,
        bytes memory data
    )
        internal
        returns(bytes memory newCtx)
    {

        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(receiver);
        if (isSuperApp &&
            (configWord & AppHelper.AFTER_AGREEMENT_CREATED_NOOP) != 0) {
            return host.callAppAfter(ctx, receiver, ISuperApp(receiver).afterAgreementUpdated.selector, data);
        }

        return ctx;
    }

    function beforeAgreementUpdated(
        ISuperfluid host,
        bytes memory ctx,
        address receiver,
        bytes memory data
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(receiver);
        if (isSuperApp &&
            (configWord & AppHelper.BEFORE_AGREEMENT_UPDATED_NOOP) != 0) {
            return host.callAppBefore(ctx, receiver, ISuperApp(receiver).beforeAgreementUpdated.selector, data);
        }

        return ("", "");
    }

    function afterAgreementUpdated(
        ISuperfluid host,
        bytes memory ctx,
        address receiver,
        bytes memory data
    )
        internal
        returns(bytes memory newCtx)
    {
        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(receiver);
        if (isSuperApp &&
            (configWord & AppHelper.AFTER_AGREEMENT_UPDATED_NOOP) != 0) {
            return host.callAppAfter(ctx, receiver, ISuperApp(receiver).afterAgreementUpdated.selector, data);
        }

        return ctx;
    }

    function beforeAgreementTerminated(
        ISuperfluid host,
        bytes memory ctx,
        address receiver,
        bytes memory data
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(receiver);
        if (isSuperApp &&
            (configWord & AppHelper.BEFORE_AGREEMENT_TERMINATED_NOOP != 0)) {
            return host.callAppBefore(ctx, receiver, ISuperApp(receiver).beforeAgreementTerminated.selector, data);
        }

        return ("", "");
    }

    function afterAgreementTerminated(
        ISuperfluid host,
        bytes memory ctx,
        address receiver,
        bytes memory data
    )
        internal
        returns(bytes memory newCtx)
    {
        (bool isSuperApp, ) = host.getAppManifest(receiver);
        if (isSuperApp) {
            return host.callAppAfter(ctx, receiver, ISuperApp(receiver).afterAgreementTerminated.selector, data);
        }

        return ctx;
    }
}
