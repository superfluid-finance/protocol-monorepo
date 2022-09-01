// SPDX-License-Identifier: UNLICENSED

pragma solidity >=0.8.4;

/**
 * @title Multisig wallet interface
 * @author Superfluid
 */
interface IMultiSigWallet {
    function submitTransaction(
        address destination,
        uint256 value,
        bytes calldata data
    ) external returns (uint256 transactionId);
}
