// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.23;

/**
 * @title DMZForwarder
 * @dev The purpose of this contract is to make arbitrary contract calls batchable.
 * We want those to be routed through this dedicated contract because having the host,
 * as msg.sender would be too dangerous.
 * This contract shall not have any special permissions.
 */
contract DMZForwarder {
    function forwardCall(address target, bytes memory data)
        external payable
        returns(bool success, bytes memory returnData)
    {
        // solhint-disable-next-line avoid-low-level-calls
        (success, returnData) = target.call(data);
    }
}