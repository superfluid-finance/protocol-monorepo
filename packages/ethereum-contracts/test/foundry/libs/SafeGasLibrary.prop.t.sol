// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { SafeGasLibrary } from "@superfluid-finance/ethereum-contracts/contracts/libs/SafeGasLibrary.sol";

import "forge-std/Test.sol";

/// @title SafeGasLibraryProperties
/// @author Superfluid
/// @notice This contract tests that the SafeGasLibrary behaves as expected.
/// @dev If the gasleft() inside the function is less than 1/63 of the gasleft() before the function call,
/// then the function ran out of gas and we revert the whole transaction.
/// We mock this here by using a multiple of the gasleft() before the function call.
contract SafeGasLibraryProperties is Test {

    function test_SafeGasRevertsAsExpected(uint16 multiple) public {
        if (multiple >= 63) {
            vm.expectRevert(SafeGasLibrary.OUT_OF_GAS.selector);
        }
        uint256 gasLeft = gasleft();
        SafeGasLibrary._revertWhenOutOfGas(gasLeft * multiple);
    }

}