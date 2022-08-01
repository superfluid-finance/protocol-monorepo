// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.14;

import { AgreementLibrary } from "@superfluid-finance/ethereum-contracts/contracts/agreements/AgreementLibrary.sol";

import "forge-std/Test.sol";

contract AgreementLibraryProperties is Test {
    function _clipDepositNumberRoundingUp(uint256 deposit)
        internal pure
        returns(uint256)
    {
        // clipping the value, rounding up
        uint256 rounding = (deposit & type(uint32).max) > 0 ? 1 : 0;
        return ((deposit >> 32) + rounding) << 32;
    }

    function getDepositDeltas(uint256 depositA, uint256 depositB)
        internal pure
        returns(uint256 clippedDelta, uint256 delta)
    {
        uint256 clippedDepositA = _clipDepositNumberRoundingUp(depositA);
        uint256 clippedDepositB = _clipDepositNumberRoundingUp(depositB);
        if (clippedDepositA > clippedDepositB) {
            delta = clippedDepositA - clippedDepositB;
            clippedDelta = _clipDepositNumberRoundingUp(delta);
        } else {
            delta = clippedDepositB - clippedDepositA;
            clippedDelta = _clipDepositNumberRoundingUp(delta);
        }
    }

    function testAdjustNewAppCreditUsed(
        uint256 appCreditGranted,
        int256 appCreditUsed
    ) public {
        vm.assume(appCreditGranted <= uint256(type(int256).max));
        vm.assume(appCreditUsed <= type(int256).max);
        int256 adjustedAppCreditUsed = AgreementLibrary._adjustNewAppCreditUsed(
            appCreditGranted,
            appCreditUsed
        );

        assertFalse(adjustedAppCreditUsed < 0);
        assertFalse(uint256(adjustedAppCreditUsed) > appCreditGranted);
    }

    /**
     * @dev This test was added to provide extra assurance that the sum of two minimum deposits is
     * equal to the clipped sum of two minimum deposits.
     * This makes sense intuitively as the last 32 bits are clipped off for both deposits so when
     * adding, nothing will ever appear in the last 32 bits.
     * This test was added because we deleted the extra clipping in the _changeFlowToApp function
     * export FOUNDRY_FUZZ_RUNS=10000 && forge test --match testMinimumDepositClippingSumInvariant
     */
    function testMinimumDepositClippingSumInvariant(
        uint256 depositA,
        uint256 depositB
    ) public {
        vm.assume(type(uint256).max - depositA < depositB);
        vm.assume(type(uint256).max - depositB < depositA);
        uint256 clippedDepositA = _clipDepositNumberRoundingUp(depositA);
        uint256 clippedDepositB = _clipDepositNumberRoundingUp(depositB);
        uint256 summedDeposit = clippedDepositA + clippedDepositB;
        uint256 clippedSummedDeposit = _clipDepositNumberRoundingUp(summedDeposit);
        assertTrue(summedDeposit == clippedSummedDeposit);
    }

    function testMinimumDepositDeltaClipping(
        uint256 depositA,
        uint256 depositB
    ) public {
        (uint256 clippedDelta, uint256 delta) = getDepositDeltas(depositA, depositB);
        assertTrue(clippedDelta == delta);
    }
}