// SPDX-License-Identifier: UNLICENSED

pragma solidity >= 0.7.4;
/**
 * @title Multisig wallet interface
 * @author Superfluid
 */
interface IMultiSigWallet {
    function submitTransaction(address destination, uint value, bytes calldata data)
        external
        returns (uint transactionId);
}
