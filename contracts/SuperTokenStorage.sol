// SPDX-License-Identifier: MIT
pragma solidity ^0.6.6;

import { IERC20 } from "./interface/ISuperToken.sol";
import { ISuperfluidGovernance } from "./interface/ISuperfluidGovernance.sol";

contract SuperTokenStorage {
    /* WARNING: NEVER RE-ORDER VARIABLES! Always double-check that new
       variables are added APPEND-ONLY. Re-ordering variables can
       permanently BREAK the deployed proxy contract. */

    /// @dev For ownable contract
    address internal _owner;

    /// @dev avoid double initialization
    bool internal _initialized;

    /// @dev ERC20 Name property
    string internal _name;
    /// @dev ERC20 Symbol property
    string internal _symbol;
    /// @dev ERC20 Decimals property
    uint8 internal _decimals;

    /// @dev The underlaying ERC20 token
    IERC20 internal _token;

    /// @dev Governance contract
    ISuperfluidGovernance internal _gov;

    /// @dev Mapping to agreement data.
    ///      Mapping order: .agreementClass.agreementID.
    ///      The generation of agreementDataID is the logic of agreement contract
    mapping(address => mapping (bytes32 => bytes)) internal _agreementData;

    /// @dev Mapping from account to agreement state of the account.
    ///      Mapping order: .agreementClass.account.
    ///      It is like RUNTIME state of the agreement for each account.
    mapping(address => mapping (address => bytes)) internal _accountStates;

    /// @dev List of enabled agreement classes for the account
    mapping(address => address[]) internal _activeAgreementClasses;

    /// @dev Settled balance for the account
    mapping(address => int256) internal _balances;

    /// @dev ERC20 Allowances Storage
    mapping (address => mapping (address => uint256)) internal _allowances;
}
