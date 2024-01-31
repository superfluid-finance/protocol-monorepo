// SPDX-License-Identifier: MIT
pragma solidity >=0.8.11;

import { ISuperfluid } from "../superfluid/ISuperfluid.sol";

// User-defined macro
interface IUserDefinedMacro {
    function executeMacro(ISuperfluid host, bytes memory params) external view
        returns (ISuperfluid.Operation[] memory operations);
}
