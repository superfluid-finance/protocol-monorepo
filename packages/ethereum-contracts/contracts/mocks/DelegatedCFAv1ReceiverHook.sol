// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperfluid, ISuperToken } from "../apps/SuperAppBase.sol";
import { IConstantFlowAgreementV1Receiver }from "../interfaces/agreements/IConstantFlowAgreementV1Receiver.sol";
import "../apps/SuperTokenV1Library.sol";
import { IERC1820Registry } from "@openzeppelin/contracts/utils/introspection/IERC1820Registry.sol";

// CFA receive hook implementation which blocks incoming flows of all but one SuperToken
contract TokenFilter is IConstantFlowAgreementV1Receiver {
    ISuperfluid public host;
    ISuperToken public goodToken;

    constructor(ISuperfluid host_, ISuperToken goodToken_) {
        host = host_;
        goodToken = goodToken_;
    }

    error BadHost();
    error BadToken();

    function onFlowChanged(
        address superToken,
        address /*operator*/,
        address /*sender*/,
        address /*receiver*/,
        int96 /*oldFlowRate*/,
        int96 newFlowRate
    ) external view override returns(bool) {
        if (msg.sender != address(host)) revert BadHost();
        if (newFlowRate != 0 && superToken != address(goodToken)) revert BadToken();
        return true;
    }
}

// Contract which sometimes wants to block all incoming flows not denominated in Pi
contract PiMaximalist {
    ISuperToken public constant PI_TOKEN = ISuperToken(0x3141592653589793238462643383279502884197);
    TokenFilter public immutable piTokenFilter;

    constructor(ISuperfluid host) {
        // the filter contract could as well already exist instead of being deployed here
        piTokenFilter = new TokenFilter(host, PI_TOKEN);
    }

    function enableFilter() external {
        IERC1820Registry reg = IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);
        reg.setInterfaceImplementer(
            address(this), keccak256("IConstantFlowAgreementV1Receiver"), address(piTokenFilter));
    }

    function disableFilter() external {
        IERC1820Registry reg = IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);
        reg.setInterfaceImplementer(
            address(this), keccak256("IConstantFlowAgreementV1Receiver"), address(0));
    }
}
