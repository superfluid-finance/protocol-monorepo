// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    CustomSuperTokenProxyBase,
    ISuperToken
}
from "../interfaces/superfluid/CustomSuperTokenProxyBase.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";


abstract contract CustomSuperTokenBaseMock is CustomSuperTokenProxyBase {
    function getFirstCustomTokenStorageSlot() external pure virtual returns (uint slot);

    function callSelfBurn(
        address to,
        uint256 amount,
        bytes memory userData
    ) external virtual;
}

// solhint-disable-next-line no-empty-blocks
abstract contract CustomSuperTokenMock is CustomSuperTokenBaseMock, ISuperToken {}

contract CustomSuperTokenProxyMock is CustomSuperTokenBaseMock, UUPSProxy {

    uint256 private _firstStorageSlot;

    function getFirstCustomTokenStorageSlot() external pure override returns (uint slot) {
        assembly { slot:= _firstStorageSlot.slot }
    }

    // this function overrides the default ISuperToken.selfMint behavior
    function selfMint(
        address to,
        uint256 amount,
        bytes memory userData
    ) external {
        // this makes msg.sender to self
        this.delegatecallSelfMint(to, amount, userData);
    }

    // this function uses delegatecall to avoid calling shadow mint defined in this contract
    function delegatecallSelfMint(
        address to,
        uint256 amount,
        bytes memory userData
    ) external {
        address logic = _implementation();
        // solhint-disable-next-line avoid-low-level-calls
        (bool success, ) = logic.delegatecall(abi.encodeWithSelector(
            ISuperToken.selfMint.selector,
            to, amount, userData));
        assert(success);
    }

    // this function self calls burn
    function callSelfBurn(
        address to,
        uint256 amount,
        bytes memory userData
    ) external override {
        // this makes msg.sender to self
        ISuperToken(address(this)).selfBurn(to, amount, userData);
    }
}
