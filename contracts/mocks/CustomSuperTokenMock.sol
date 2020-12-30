// SPDX-License-Identifier: MIT
pragma solidity 0.7.6;

import {
    ISuperfluid,
    ISuperAgreement,
    ISuperToken
} from "../superfluid/SuperToken.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";

import { CallUtils } from "../utils/CallUtils.sol";


interface CustomSuperTokenFunctionsMock {
    function selfBurn(
        address to,
        uint256 amount,
        bytes memory userData
    ) external;
}

// solhint-disable-next-line no-empty-blocks
interface CustomSuperTokenMock is CustomSuperTokenFunctionsMock, ISuperToken { }

contract CustomSuperTokenProxyMock is CustomSuperTokenFunctionsMock, UUPSProxy {

    // this function shadows ISuperToken.mint with selfMint
    function mint(
        address to,
        uint256 amount,
        bytes memory userData
    ) external {
        // this makes msg.sender to self
        this.selfMint(to, amount, userData);
    }

    // this function uses delegatecall to avoid calling shadow mint defined in this contract
    function selfMint(
        address to,
        uint256 amount,
        bytes memory userData
    ) external {
        address logic = _implementation();
        bool success;
        bytes memory returnedValue;
        // solhint-disable-next-line avoid-low-level-calls
        (success, returnedValue) = logic.delegatecall(abi.encodeWithSelector(
            ISuperToken.mint.selector,
            to, amount, userData));
        if (!success) revert(CallUtils.getRevertMsg(returnedValue));
    }

    // this function self calls burn
    function selfBurn(
        address to,
        uint256 amount,
        bytes memory userData
    ) external override {
        // this makes msg.sender to self
        ISuperToken(address(this)).burn(to, amount, userData);
    }
}
