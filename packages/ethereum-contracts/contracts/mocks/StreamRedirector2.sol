// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperToken } from "../apps/SuperAppBase.sol";
import { ConstantFlowAgreementV1ReceiveHook }from "../agreements/ConstantFlowAgreementV1ReceiveHook.sol";
import "../apps/SuperTokenV1Library.sol";

contract StreamRedirector2 is ConstantFlowAgreementV1ReceiveHook {
    using SuperTokenV1Library for ISuperToken;

    address public redirectReceiver; // flow is redirected to hardcoded receiver address
    ISuperToken public acceptedToken; // accepted super token

    constructor(
        ISuperToken _acceptedToken,
        address _redirectReceiver
    ) {
        assert(address(_acceptedToken) != address(0));
        assert(address(_redirectReceiver) != address(0));

        acceptedToken = _acceptedToken;
        redirectReceiver = _redirectReceiver;
    }

    error UnsupportedToken();

    function onFlowChanged(
        address /*operator*/,
        address /*sender*/,
        address /*receiver*/,
        int96 oldFlowRate,
        int96 newFlowRate
    ) external override returns(bool) {
        if (msg.sender != address(acceptedToken)) revert UnsupportedToken();

        if (oldFlowRate == 0 && newFlowRate > 0) {
            acceptedToken.createFlow(redirectReceiver, newFlowRate);
        } else if (oldFlowRate > 0 && newFlowRate == 0) {
            acceptedToken.deleteFlow(address(this), redirectReceiver);
        }

        return true;
    }
}
