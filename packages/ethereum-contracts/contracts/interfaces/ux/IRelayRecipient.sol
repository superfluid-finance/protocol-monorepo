// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.7.0;

// MODIFIED FROM: https://github.com/opengsn/forwarder/blob/master/contracts/interfaces/IRelayRecipient.sol

/**
 * a contract must implement this interface in order to support relayed transaction.
 * It is better to inherit the BaseRelayRecipient as its implementation.
 */
interface IRelayRecipient {

    /**
     * return if the forwarder is trusted to forward relayed transactions to us.
     * the forwarder is required to verify the sender's signature, and verify
     * the call is not a replay.
     */
    function isTrustedForwarder(address forwarder) external view returns(bool);

    function versionRecipient() external view returns (string memory);
}
