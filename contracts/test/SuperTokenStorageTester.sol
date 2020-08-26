// SPDX-License-Identifier: MIT
/* solhint-disable */
pragma solidity 0.7.0;
pragma experimental ABIEncoderV2;

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

        // string internal _name;
        assembly { slot:= _name.slot offset := _name.offset }
        require (slot == 1 && offset == 0, "_name changed location");

        // string internal _symbol;
        assembly { slot:= _symbol.slot offset := _symbol.offset }
        require (slot == 2 && offset == 0, "_symbol changed location");

        // uint8 internal _decimals;
        assembly { slot:= _decimals.slot offset := _decimals.offset }
        require (slot == 3 && offset == 0, "_decimals changed location");

        // IERC20 internal _token;
        assembly { slot:= _token.slot offset := _token.offset }
        require (slot == 3 && offset == 1, "_token changed location");

        // ISuperfluidGovernance internal _gov
        assembly { slot:= _gov.slot offset := _gov.offset }
        require (slot == 4 && offset == 0, "_gov changed location");

        // mapping(address => mapping (bytes32 => bytes)) internal _agreementData;
        assembly { slot:= _agreementData.slot offset := _agreementData.offset }
        require (slot == 5 && offset == 0, "_agreementData changed location");

        // mapping(address => mapping (address => bytes)) internal _accountStates;
        assembly { slot:= _accountStates.slot offset := _accountStates.offset }
        require (slot == 6 && offset == 0, "_accountStates changed location");

        // mapping(address => address[]) internal _activeAgreementClasses;
        assembly { slot:= _activeAgreementClasses.slot offset := _activeAgreementClasses.offset }
        require (slot == 7 && offset == 0, "_activeAgreementClasses changed location");

        // mapping(address => int256) internal _balances;
        assembly { slot:= _balances.slot offset := _balances.offset }
        require (slot == 8 && offset == 0, "_balances changed location");

        // mapping (address => mapping (address => uint256)) internal _allowances;
        assembly { slot:= _allowances.slot offset := _allowances.offset }
        require (slot == 9 && offset == 0, "_allowances changed location");
    }
}
