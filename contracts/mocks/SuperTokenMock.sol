// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import {
    ISuperAgreement,
    SuperToken
} from "../superfluid/SuperToken.sol";


contract SuperTokenMock is SuperToken {

    uint256 immutable public waterMark;

    constructor(uint256 w) {
        waterMark = w;
    }

    /**
     * Upgradability
     */

    function validateStorageLayout() external pure {
        uint256 slot;
        uint256 offset;

        assembly { slot:= _host.slot offset := _host.offset }
        require (slot == 0 && offset == 2, "_host changed location");

        assembly { slot:= _inactiveAgreementBitmap.slot offset := _inactiveAgreementBitmap.offset }
        require (slot == 1 && offset == 0, "_inactiveAgreementBitmap changed location");

        assembly { slot:= _balances.slot offset := _balances.offset }
        require (slot == 2 && offset == 0, "_balances changed location");

        assembly { slot:= _underlyingToken.slot offset := _underlyingToken.offset }
        require (slot == 3 && offset == 0, "_underlyingToken changed location");

        assembly { slot:= _underlyingDecimals.slot offset := _underlyingDecimals.offset }
        require (slot == 3 && offset == 20, "_underlyingDecimals changed location");

        assembly { slot:= _name.slot offset := _name.offset }
        require (slot == 4 && offset == 0, "_name changed location");

        assembly { slot:= _symbol.slot offset := _symbol.offset }
        require (slot == 5 && offset == 0, "_symbol changed location");

        assembly { slot:= _allowances.slot offset := _allowances.offset }
        require (slot == 6 && offset == 0, "_allowances changed location");

        assembly { slot:= _operators.slot offset := _operators.offset }
        require (slot == 7 && offset == 0, "_operators changed location");
    }

    /**
     * ERC-20 mockings
     */
    function approveInternal(address owner, address spender, uint256 value) public {
        _approve(owner, spender, value);
    }

    function transferInternal(address from, address to, uint256 value) public {
        _transferFrom(from, from, to, value);
    }

    /**
     * ERC-777 mockings
     */
    function setupDefaultOperators(address[] memory operators) public {
        _setupDefaultOperators(operators);
    }

    function mintInternal(
        address to,
        uint256 amount,
        bytes memory userData,
        bytes memory operatorData
    ) public {
        // set requireReceptionAck to true always
        _mint(msg.sender, to, amount, true, userData, operatorData);
    }

}
