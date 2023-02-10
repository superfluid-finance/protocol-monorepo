// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import { AgreementLibrary } from "@superfluid-finance/ethereum-contracts/contracts/agreements/AgreementLibrary.sol";

import "forge-std/Test.sol";

contract AgreementLibraryProperties is Test {

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

}