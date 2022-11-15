// SPDX-License-Identifier: UNLICENSED

pragma solidity >= 0.8.4;
/**
 * @title Multisig wallet interface
 * @author Superfluid
 */
interface IMultiSigWallet {
    function submitTransaction(address destination, uint value, bytes calldata data)
        external
        returns (uint transactionId);

    // used for interface probing
    function required()
        external view
        returns (uint256);
}
