// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

/// @title SafeGasLibrary
/// @author Superfluid
/// @notice An internal library used to handle out of gas errors
library SafeGasLibrary {
    error OUT_OF_GAS();

    /// @dev A function used in the catch block to handle true out of gas errors
    /// @param gasLeftBefore the gas left before the try/catch block
    function _revertWhenOutOfGas(uint256 gasLeftBefore) internal {
// If the function actually runs out of gas, not just hitting the safety gas limit, we revert the whole transaction.
// This solves an issue where the gas estimaton didn't provide enough gas by default for the function to succeed.
// See https://medium.com/@wighawag/ethereum-the-concept-of-gas-and-its-dangers-28d0eb809bb2
        if (gasleft() <= gasLeftBefore / 63) {
            revert OUT_OF_GAS();
        }
    }
}
