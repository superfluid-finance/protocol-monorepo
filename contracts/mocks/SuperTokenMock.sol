// SPDX-License-Identifier: MIT
pragma solidity 0.7.6;

import {
    ISuperfluid,
    ISuperAgreement,
    SuperToken
} from "../superfluid/SuperToken.sol";


contract SuperTokenMock is SuperToken {

    uint256 immutable public waterMark;

    constructor(ISuperfluid host, uint256 w)
        SuperToken(host)
    {
        waterMark = w;
    }

    /**
     * Upgradability
     */

    function validateStorageLayout() external pure {
        uint256 slot;
        uint256 offset;

        // Initializable _initialized and _initialized

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
        // uses 4 slots

        assembly { slot:= _reserve0.slot offset := _reserve0.offset }
        require (slot == 11 && offset == 0, "_reserve0 changed location");

        assembly { slot:= _reserve9.slot offset := _reserve9.offset }
        require (slot == 20 && offset == 0, "_reserve9 changed location");
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
