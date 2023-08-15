// SPDX-License-Identifier: UNLICENSED

pragma solidity >= 0.8.11;
/**
 * @title Partial Multisig wallet interface
 * See https://github.com/gnosis/MultiSigWallet/blob/master/contracts/MultiSigWallet.sol
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
