// SPDX-License-Identifier: UNLICENSED

pragma solidity >= 0.4.22;

interface IMultiSigWallet {
    function submitTransaction(address destination, uint value, bytes calldata data)
        external
        returns (uint transactionId);
}
