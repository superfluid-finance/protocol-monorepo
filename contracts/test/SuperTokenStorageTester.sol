// SPDX-License-Identifier: MIT
pragma solidity ^0.6.6;
pragma experimental ABIEncoderV2;

import {SuperTokenStorage} from "../SuperTokenStorage.sol";

/**
 * @dev Test SuperTokenStorag Layout changes
 */
contract SuperTokenStorageTester is SuperTokenStorage {
    function validate() public pure {
        uint256 slot;
        uint256 offset;

        // address public _owner;
        assembly { slot:= _owner_slot offset := _owner_offset }
        require (slot == 0 && offset == 0, "_owner changed location");

        // bool public initialized;
        assembly { slot:= _initialized_slot offset := _initialized_offset }
        require (slot == 0 && offset == 20, "initialized changed location");

        // string internal _name;
        assembly { slot:= _name_slot offset := _name_offset }
        require (slot == 1 && offset == 0, "_name changed location");

        // string internal _symbol;
        assembly { slot:= _symbol_slot offset := _symbol_offset }
        require (slot == 2 && offset == 0, "_symbol changed location");

        // uint8 internal _decimals;
        assembly { slot:= _decimals_slot offset := _decimals_offset }
        require (slot == 3 && offset == 0, "_decimals changed location");

        // IERC20 internal _token;
        assembly { slot:= _token_slot offset := _token_offset }
        require (slot == 3 && offset == 1, "_token changed location");

        // ISuperfluidGovernance internal _gov
        assembly { slot:= _gov_slot offset := _gov_offset }
        require (slot == 4 && offset == 0, "_gov changed location");

        // mapping(address => mapping (bytes32 => bytes)) internal _agreementData;
        assembly { slot:= _agreementData_slot offset := _agreementData_offset }
        require (slot == 5 && offset == 0, "_agreementData changed location");

        // mapping(address => mapping (address => bytes)) internal _accountStates;
        assembly { slot:= _accountStates_slot offset := _accountStates_offset }
        require (slot == 6 && offset == 0, "_accountStates changed location");

        // mapping(address => address[]) internal _activeAgreementClasses;
        assembly { slot:= _activeAgreementClasses_slot offset := _activeAgreementClasses_offset }
        require (slot == 7 && offset == 0, "_activeAgreementClasses changed location");

        // mapping(address => int256) internal _balances;
        assembly { slot:= _balances_slot offset := _balances_offset }
        require (slot == 8 && offset == 0, "_balances changed location");

        // mapping (address => mapping (address => uint256)) internal _allowances;
        assembly { slot:= _allowances_slot offset := _allowances_offset }
        require (slot == 9 && offset == 0, "_allowances changed location");
    }
}
