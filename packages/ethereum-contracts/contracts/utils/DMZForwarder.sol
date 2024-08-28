// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.23;

import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";

/**
 * @title DMZForwarder
 * @dev The purpose of this contract is to make arbitrary contract calls batchable
 * alongside Superfluid specific batch operations.
 * We route the calls through this dedicated contract in order to not have msg.sender set
 * to the host contract, for security reasons.
 * Forwarded calls can optionally use ERC-2771 to preserve the original msg.sender.
 * If native tokens (msg.value) are provided, they are forwarded as well.
 */
contract DMZForwarder is Ownable {
    /**
     * @dev Forwards a call for which msg.sender doesn't matter
     * @param target The target contract to call
     * @param data The call data
     */
    function forwardCall(address target, bytes memory data)
        external payable
        returns(bool success, bytes memory returnData)
    {
        // solhint-disable-next-line avoid-low-level-calls
        (success, returnData) = target.call{value: msg.value}(data);
    }

    /**
     * @dev Forwards a call passing along the original msg.sender encoded as specified in ERC-2771.
     * @param target The target contract to call
     * @param msgSender The original msg.sender passed along by the trusted contract owner
     * @param data The call data
     */
    function forward2771Call(address target, address msgSender, bytes memory data)
        external payable onlyOwner
        returns(bool success, bytes memory returnData)
    {
        // solhint-disable-next-line avoid-low-level-calls
        (success, returnData) = target.call{value: msg.value}(abi.encodePacked(data, msgSender));
    }

    /**
     * @dev Allows to withdraw native tokens (ETH) which got stuck in this contract.
     * This could happen if a call fails, but the caller doesn't revert the tx.
     */
    function withdrawLostNativeTokens(address payable receiver) external onlyOwner {
        receiver.transfer(address(this).balance);
    }
}