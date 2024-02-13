// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.23;

import { IUserDefinedMacro } from "../interfaces/utils/IUserDefinedMacro.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ForwarderBase } from "../utils/ForwarderBase.sol";


/**
 * @dev This is a trusted forwarder with high degree of extensibility through permission-less and user-defined "macro
 * contracts". This is a vanilla version without EIP-712 support.
 */
contract MacroForwarder is ForwarderBase {
    constructor(ISuperfluid host) ForwarderBase(host) {}

    /**
     * @dev Run the macro defined by the provided macro contract and params.
     * @param  m          Target macro.
     * @param  params     Parameters to run the macro.
     */
    function runMacro(IUserDefinedMacro m, bytes memory params) external returns (bool)
    {
        ISuperfluid.Operation[] memory operations = m.buildBatchOperations(_host, params);
        return _forwardBatchCall(operations);
    }
}
