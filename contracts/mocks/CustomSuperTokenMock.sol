// SPDX-License-Identifier: MIT
pragma solidity 0.7.6;

import {
    ISuperfluid,
    ISuperAgreement,
    SuperToken
} from "../superfluid/SuperToken.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";

import { CallUtils } from "../utils/CallUtils.sol";


contract CustomSuperTokenMock is UUPSProxy {

    // this function shadows SuperToken.mint with selfMint
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
            SuperToken.mint.selector,
            to, amount, userData));
        if (!success) revert(CallUtils.getRevertMsg(returnedValue));
    }
}
