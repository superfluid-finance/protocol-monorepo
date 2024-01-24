// SPDX-License-Identifier: MIT
pragma solidity >=0.8.11;

import { ISuperfluid } from "../superfluid/ISuperfluid.sol";

/**
 * @dev User-defined macro used in implementations of TrustedMacros.
 */
interface IUserDefinedMacro {
    /**
     * @dev Build batch operations according the parameters provided by the host contract.
     * @param  host       The executing host contract.
     * @param  params     The encoded form of the parameters.
     * @return operations The batch operations built.
     */
    function buildBatchOperations(ISuperfluid host, bytes memory params) external view
        returns (ISuperfluid.Operation[] memory operations);
}
