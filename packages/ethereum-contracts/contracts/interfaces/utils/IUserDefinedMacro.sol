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
}
