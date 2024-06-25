// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.23;

import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";

/**
 * @title DMZForwarder
 * @dev The purpose of this contract is to make arbitrary contract calls batchable
 * alongside Superfluid specific batch operations.
 * We route the callse through this dedicated contract in order to not have msg.sender set
 * to the host contract, for security reasons.
 * Forwarded calls can optionally use ERC-2771 to preserve the original msg.sender.
 */
contract DMZForwarder is Ownable {
    function forwardCall(address target, bytes memory data)
        external
        returns(bool success, bytes memory returnData)
    {
        // solhint-disable-next-line avoid-low-level-calls
        (success, returnData) = target.call(data);
    }

    // Forwards a call using ERC-2771.
    // That means the original msg.sender (provided by the host) is passed along in a trusted way.
    // A recipient contract needs to trust this contract (trusted forwarder).
    function forward2771Call(address target, address msgSender, bytes memory data)
        external onlyOwner
        returns(bool success, bytes memory returnData)
    {
        // solhint-disable-next-line avoid-low-level-calls
        (success, returnData) = target.call(abi.encodePacked(data, msgSender));
    }
}