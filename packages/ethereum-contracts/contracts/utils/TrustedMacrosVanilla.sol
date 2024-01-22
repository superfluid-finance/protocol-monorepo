// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IUserDefinedMacro } from "../interfaces/utils/IUserDefinedMacro.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ForwarderBase } from "../utils/ForwarderBase.sol";


/**
 * @dev This is a trusted forwarder with high degree of extensibility through permission-less and user-defined "macro
 * contracts". This is a vanilla version without EIP-712 support.
 */
contract TrustedMacrosVanilla is ForwarderBase {
    constructor(ISuperfluid host) ForwarderBase(host) {}

    function simulateMacro(IUserDefinedMacro m, bytes memory params) public view
        returns (ISuperfluid.Operation[] memory operations)
    {
        operations = m.executeMacro(_host, params);
    }

    function runMacro(IUserDefinedMacro m, bytes memory params) external returns (bool)
    {
        ISuperfluid.Operation[] memory operations = simulateMacro(m, params);
        return _forwardBatchCall(operations);
    }
}
