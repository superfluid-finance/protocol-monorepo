// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import { ISuperToken } from "../superfluid/ISuperToken.sol";

/**
 * @title Matic Bridged Native SuperToken Custom interface
 * @author Superfluid
 * @dev Functionality specific for Matic Bridged Native Super Tokens
 */
interface IMaticBridgedNativeSuperTokenCustom {
    /// @dev triggers minting of tokens to the given user, called by the child chain manager
    function deposit(address user, bytes calldata depositData) external;

    /// @dev triggers burning of tokens on the child chain and unlocking on L1
    function withdraw(uint256 amount) external;

    /// @dev governance can change the child chain manager
    function updateChildChainManager(address newChildChainManager) external;

    /// @dev emitted when the child chain manager changes
    event ChildChainManagerChanged(address newAddress);
}

/**
 * @dev Matic Bridged Native SuperToken full interface
 * @author Superfluid
 */
interface IMaticBridgedNativeSuperToken is IMaticBridgedNativeSuperTokenCustom, ISuperToken {}
