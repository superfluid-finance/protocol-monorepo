// SPDX-License-Identifier: MIT
pragma solidity >=0.8.11;

import { ISuperfluid } from "../superfluid/ISuperfluid.sol";

/**
 * @dev User-defined macro used in implementations of TrustedMacros.
 */
interface IUserDefinedMacro {
    /**
     * @dev Build batch operations according to the parameters provided.
     * It's up to the macro contract to map the provided params (can also be empty) to any
     * valid list of operations.
     * @param  host       The executing host contract.
     * @param  params     The encoded form of the parameters.
     * @param  msgSender  The msg.sender of the call to the MacroForwarder.
     * @return operations The batch operations built.
     */
    function buildBatchOperations(ISuperfluid host, bytes memory params, address msgSender) external view
        returns (ISuperfluid.Operation[] memory operations);

    /**
     * @dev A post-check function which is called after execution.
     * It allows to do arbitrary checks based on the state after execution,
     * and to revert if the result is not as expected.
     * Can be an empty implementation if no check is needed.
     */
    function postCheck() external view;

    /*
     * Additional to the required interface, we recommend to implement the following function:
     * `function getParams(...) external view returns (bytes memory);`
     *
     * It shall return abi encoded params as required as second argument of `MacroForwarder.runMacro()`.
     *
     * The function name shall be `getParams` and the return type shall be `bytes memory`.
     * The number, type and name of arguments are free to choose such that they best fit the macro use case.
     *
     * In conjunction with the name of the Macro contract, the signature should be as self-explanatory as possible.
     *
     * Example for a contract `MultiFlowDeleteMacro` which lets a user delete multiple flows in one transaction:
     * `function getParams(ISuperToken superToken, address[] memory receivers) external view returns (bytes memory)`
     *
     *
     * Implementing this view function has several advantages:
     * - Allows to use generic tooling like Explorers to interact with the macro
     * - Allows to build auto-generated UIs based on the contract ABI
     * - Makes it easier to interface with the macro from Dapps
     *
     */
}
