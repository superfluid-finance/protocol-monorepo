// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";
import { SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import { SuperTokenLibraryCFAMock } from "../../../contracts/mocks/SuperTokenLibraryV1Mock.sol";

contract SuperTokenV1LibraryIntegrationTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;

    SuperTokenLibraryCFAMock internal superTokenLibraryCFAMock;

    constructor() FoundrySuperfluidTester(3) { }

    function setUp() public override {
        super.setUp();
        superTokenLibraryCFAMock = new SuperTokenLibraryCFAMock();
    }

    function testIncreaseFlowRateAllowance(address flowOperator, int96 addedFlowRateAllowance) public {
        vm.assume(addedFlowRateAllowance >= 0);

        superTokenLibraryCFAMock.increaseFlowRateAllowanceTest(superToken, flowOperator, addedFlowRateAllowance);

        (,,, int96 allowance) = superToken.getFlowPermissions(address(superTokenLibraryCFAMock), flowOperator);

        assertEq(allowance, addedFlowRateAllowance);
    }

    function testIncreaseFlowRateAllowanceWithUserData(
        address flowOperator,
        int96 addedFlowRateAllowance,
        bytes memory userData
    ) public {
        vm.assume(addedFlowRateAllowance >= 0);

        superTokenLibraryCFAMock.increaseFlowRateAllowanceWithUserDataTest(
            superToken, flowOperator, addedFlowRateAllowance, userData
        );

        (,,, int96 allowance) = superToken.getFlowPermissions(address(superTokenLibraryCFAMock), flowOperator);

        assertEq(allowance, addedFlowRateAllowance);
    }

    function testDecreaseFlowRateAllowance(
        address flowOperator,
        int96 addedFlowRateAllowance,
        int96 subtractedFlowRateAllowance
    ) public {
        vm.assume(subtractedFlowRateAllowance >= 0);
        vm.assume(addedFlowRateAllowance >= subtractedFlowRateAllowance);

        superTokenLibraryCFAMock.increaseFlowRateAllowanceTest(superToken, flowOperator, addedFlowRateAllowance);
        superTokenLibraryCFAMock.decreaseFlowRateAllowanceTest(superToken, flowOperator, subtractedFlowRateAllowance);

        (,,, int96 allowance) = superToken.getFlowPermissions(address(superTokenLibraryCFAMock), flowOperator);

        assertEq(allowance, addedFlowRateAllowance - subtractedFlowRateAllowance);
    }

    function testDecreaseFlowRateAllowanceWithUserData(
        address flowOperator,
        int96 addedFlowRateAllowance,
        int96 subtractedFlowRateAllowance
    ) public {
        vm.assume(subtractedFlowRateAllowance >= 0);
        vm.assume(addedFlowRateAllowance >= subtractedFlowRateAllowance);

        superTokenLibraryCFAMock.increaseFlowRateAllowanceTest(superToken, flowOperator, addedFlowRateAllowance);
        superTokenLibraryCFAMock.decreaseFlowRateAllowanceTest(superToken, flowOperator, subtractedFlowRateAllowance);

        (,,, int96 allowance) = superToken.getFlowPermissions(address(superTokenLibraryCFAMock), flowOperator);

        assertEq(allowance, addedFlowRateAllowance - subtractedFlowRateAllowance);
    }
}
