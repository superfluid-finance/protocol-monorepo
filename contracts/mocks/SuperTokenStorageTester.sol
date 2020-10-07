// SPDX-License-Identifier: MIT
/* solhint-disable */
pragma solidity 0.7.1;

import {SuperToken} from "../superfluid/SuperToken.sol";

/**
 * @dev Test SuperTokenStorag Layout changes
 */
contract SuperTokenStorageTester is SuperToken {
    function validate() public pure {
        uint256 slot;
        uint256 offset;

        // address public _owner;
        assembly { slot:= _owner.slot offset := _owner.offset }
        require (slot == 0 && offset == 0, "_owner changed location");

        // bool public initialized;
        assembly { slot:= _initialized.slot offset := _initialized.offset }
        require (slot == 0 && offset == 20, "initialized changed location");

        // IERC20 internal _token;
        assembly { slot:= _underlyingToken.slot offset := _underlyingToken.offset }
        require (slot == 1 && offset == 0, "_underlyingToken changed location");

        // uint8 internal _decimals;
        assembly { slot:= _underlyingDecimals.slot offset := _underlyingDecimals.offset }
        require (slot == 1 && offset == 20, "_underlyingDecimals changed location");

        // string internal _name;
        assembly { slot:= _name.slot offset := _name.offset }
        require (slot == 2 && offset == 0, "_name changed location");

        // string internal _symbol;
        assembly { slot:= _symbol.slot offset := _symbol.offset }
        require (slot == 3 && offset == 0, "_symbol changed location");

        // ISuperfluid internal _host
        assembly { slot:= _host.slot offset := _host.offset }
        require (slot == 4 && offset == 0, "_host changed location");

        // mapping(address => int256) internal _balances;
        assembly { slot:= _balances.slot offset := _balances.offset }
        require (slot == 5 && offset == 0, "_balances changed location");

        // mapping (address => mapping (address => uint256)) internal _allowances;
        assembly { slot:= _allowances.slot offset := _allowances.offset }
        require (slot == 6 && offset == 0, "_allowances changed location");

        // mapping(address => address[]) internal _inactiveAgreementBitmap;
        assembly { slot:= _inactiveAgreementBitmap.slot offset := _inactiveAgreementBitmap.offset }
        require (slot == 7 && offset == 0, "_inactiveAgreementBitmap changed location");

        // mapping(address => mapping(address => bool)) internal _operators;
        assembly { slot:= _operators.slot offset := _operators.offset }
        require (slot == 8 && offset == 0, "_operators changed location");
    }
}
