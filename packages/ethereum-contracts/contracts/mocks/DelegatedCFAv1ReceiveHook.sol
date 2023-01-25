// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperToken } from "../apps/SuperAppBase.sol";
import { IConstantFlowAgreementV1ReceiveHook } from "../interfaces/agreements/IConstantFlowAgreementV1ReceiveHook.sol";
import "../apps/SuperTokenV1Library.sol";
import { IERC1820Registry } from "@openzeppelin/contracts/utils/introspection/IERC1820Registry.sol";

// CFA receive hook implementation which blocks incoming flows of all but one SuperToken
contract TokenFilter is IConstantFlowAgreementV1ReceiveHook {
    ISuperToken public goodToken;

    constructor(ISuperToken goodToken_) {
        goodToken = goodToken_;
    }

    error BadToken();

    function onFlowChanged(
        address /*operator*/,
        address /*sender*/,
        address /*receiver*/,
        int96 /*oldFlowRate*/,
        int96 newFlowRate
    ) external view override returns(bool) {
        if (newFlowRate != 0 && msg.sender != address(goodToken)) revert BadToken();
        return true;
    }
}

// Contract which sometimes wants to block all incoming flows not denominated in Pi
contract PiMaximalist {
    ISuperToken public constant PI_TOKEN = ISuperToken(0x3141592653589793238462643383279502884197);
    TokenFilter public immutable piTokenFilter;

    constructor() {
        // the filter contract could as well already exist instead of being deployed here
        piTokenFilter = new TokenFilter(PI_TOKEN);
    }

    function enableFilter() external {
        IERC1820Registry reg = IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);
        reg.setInterfaceImplementer(
            address(this), keccak256("IConstantFlowAgreementV1ReceiveHook"), address(piTokenFilter));
    }

    function disableFilter() external {
        IERC1820Registry reg = IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);
        reg.setInterfaceImplementer(
            address(this), keccak256("IConstantFlowAgreementV1ReceiveHook"), address(0));
    }
}
