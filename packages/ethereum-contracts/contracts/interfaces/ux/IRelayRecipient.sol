// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

// MODIFIED FROM: https://github.com/opengsn/forwarder/blob/master/contracts/interfaces/IRelayRecipient.sol

/**
 * @title Relay recipient interface
 * @author Superfluid
 * @dev A contract must implement this interface in order to support relayed transactions
 * @dev It is better to inherit the BaseRelayRecipient as its implementation
 */
interface IRelayRecipient {

    /**
     * @notice Returns if the forwarder is trusted to forward relayed transactions to us.
     * @dev the forwarder is required to verify the sender's signature, and verify
     *      the call is not a replay.
     */
    function isTrustedForwarder(address forwarder) external view returns(bool);

    /**
     * @dev EIP 2771 version
     *
     * NOTE:
     * - It is not clear if it is actually from the EIP 2771....
     * - https://docs.biconomy.io/guides/enable-gasless-transactions/eip-2771
     */
    function versionRecipient() external view returns (string memory);
}
