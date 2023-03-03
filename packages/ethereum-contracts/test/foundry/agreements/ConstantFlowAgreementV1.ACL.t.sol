// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import { SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import {
    IConstantFlowAgreementV1
} from "../../../contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import {
    ISuperfluidToken
} from "../../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";

contract ConstantFlowAgreementV1ACLTests is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;

    constructor() FoundrySuperfluidTester(3) {}

    function helper_Increase_Flow_Rate_Allowance(
        ISuperfluidToken _superToken,
        address flowOperator,
        int96 flowRateAllowanceDelta
    ) internal {
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(
                sf.cfa.increaseFlowRateAllowance,
                (
                    _superToken,
                    flowOperator,
                    flowRateAllowanceDelta,
                    new bytes(0)
                )
            ),
            new bytes(0)
        );
    }

    function helper_Decrease_Flow_Rate_Allowance(
        ISuperfluidToken _superToken,
        address flowOperator,
        int96 flowRateAllowanceDelta
    ) internal {
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(
                sf.cfa.decreaseFlowRateAllowance,
                (
                    _superToken,
                    flowOperator,
                    flowRateAllowanceDelta,
                    new bytes(0)
                )
            ),
            new bytes(0)
        );
    }

    function test_Passing_Increase_Flow_Rate_Allowance(
        int96 flowRateAllowanceDelta
    ) public {
        vm.assume(flowRateAllowanceDelta > 0);

        (
            bytes32 oldFlowOperatorId,
            uint8 oldPermissions,
            int96 oldFlowRateAllowance
        ) = sf.cfa.getFlowOperatorData(superToken, alice, bob);

        vm.startPrank(alice);
        helper_Increase_Flow_Rate_Allowance(
            superToken,
            bob,
            flowRateAllowanceDelta
        );
        vm.stopPrank();
        (
            bytes32 newFlowOperatorId,
            uint8 newPermissions,
            int96 newFlowRateAllowance
        ) = sf.cfa.getFlowOperatorData(superToken, alice, bob);

        assertEq(oldFlowOperatorId, newFlowOperatorId);
        assertEq(oldPermissions, newPermissions);
        assertEq(
            oldFlowRateAllowance + flowRateAllowanceDelta,
            newFlowRateAllowance
        );
    }

    function test_Passing_Decrease_Flow_Rate_Allowance(
        int96 flowRateAllowanceIncreaseDelta,
        int96 flowRateAllowanceDecreaseDelta
    ) public {
        vm.assume(flowRateAllowanceIncreaseDelta > 0);
        vm.assume(flowRateAllowanceDecreaseDelta > 0);
        vm.assume(
            flowRateAllowanceDecreaseDelta <= flowRateAllowanceIncreaseDelta
        );

        (
            bytes32 oldFlowOperatorId,
            uint8 oldPermissions,
            int96 oldFlowRateAllowance
        ) = sf.cfa.getFlowOperatorData(superToken, alice, bob);

        vm.startPrank(alice);
        helper_Increase_Flow_Rate_Allowance(
            superToken,
            bob,
            flowRateAllowanceIncreaseDelta
        );
        helper_Decrease_Flow_Rate_Allowance(
            superToken,
            bob,
            flowRateAllowanceDecreaseDelta
        );
        vm.stopPrank();

        (
            bytes32 newFlowOperatorId,
            uint8 newPermissions,
            int96 newFlowRateAllowance
        ) = sf.cfa.getFlowOperatorData(superToken, alice, bob);

        assertEq(oldFlowOperatorId, newFlowOperatorId);
        assertEq(oldPermissions, newPermissions);
        assertEq(
            oldFlowRateAllowance +
                flowRateAllowanceIncreaseDelta -
                flowRateAllowanceDecreaseDelta,
            newFlowRateAllowance
        );
    }

    function test_Passing_Increase_Flow_Rate_Allowance_With_Pre_Set_Permissions(
        int96 flowRateAllowanceDelta
    ) public {
        vm.assume(flowRateAllowanceDelta > 0);

        vm.startPrank(alice);
        superToken.setFlowPermissions(bob, true, true, true, 0);

        (
            bytes32 oldFlowOperatorId,
            uint8 oldPermissions,
            int96 oldFlowRateAllowance
        ) = sf.cfa.getFlowOperatorData(superToken, alice, bob);

        helper_Increase_Flow_Rate_Allowance(
            superToken,
            bob,
            flowRateAllowanceDelta
        );
        vm.stopPrank();
        (
            bytes32 newFlowOperatorId,
            uint8 newPermissions,
            int96 newFlowRateAllowance
        ) = sf.cfa.getFlowOperatorData(superToken, alice, bob);

        assertEq(oldFlowOperatorId, newFlowOperatorId);
        assertEq(oldPermissions, newPermissions);
        assertEq(
            oldFlowRateAllowance + flowRateAllowanceDelta,
            newFlowRateAllowance
        );
    }

    function test_Passing_Decrease_Flow_Rate_Allowance_With_Pre_Set_Permissions(
        int96 flowRateAllowanceIncreaseDelta,
        int96 flowRateAllowanceDecreaseDelta
    ) public {
        vm.assume(flowRateAllowanceIncreaseDelta > 0);
        vm.assume(flowRateAllowanceDecreaseDelta > 0);
        vm.assume(
            flowRateAllowanceDecreaseDelta <= flowRateAllowanceIncreaseDelta
        );

        vm.startPrank(alice);
        superToken.setFlowPermissions(bob, true, true, true, 0);

        (
            bytes32 oldFlowOperatorId,
            uint8 oldPermissions,
            int96 oldFlowRateAllowance
        ) = sf.cfa.getFlowOperatorData(superToken, alice, bob);

        helper_Increase_Flow_Rate_Allowance(
            superToken,
            bob,
            flowRateAllowanceIncreaseDelta
        );
        helper_Decrease_Flow_Rate_Allowance(
            superToken,
            bob,
            flowRateAllowanceDecreaseDelta
        );
        vm.stopPrank();

        (
            bytes32 newFlowOperatorId,
            uint8 newPermissions,
            int96 newFlowRateAllowance
        ) = sf.cfa.getFlowOperatorData(superToken, alice, bob);

        assertEq(oldFlowOperatorId, newFlowOperatorId);
        assertEq(oldPermissions, newPermissions);
        assertEq(
            oldFlowRateAllowance +
                flowRateAllowanceIncreaseDelta -
                flowRateAllowanceDecreaseDelta,
            newFlowRateAllowance
        );
    }

    function test_Passing_Increase_Flow_Rate_Allowance_And_ACL_Create_Flow(
        uint32 flowRateAllowanceDelta
    ) public {
        vm.assume(flowRateAllowanceDelta > 0);
        vm.assume(flowRateAllowanceDelta <= uint32(type(int32).max));
        int96 flowRate = int96(int32(flowRateAllowanceDelta));
        vm.assume(flowRateAllowanceDelta > 0);

        vm.startPrank(alice);
        superToken.setFlowPermissions(bob, true, true, true, 0);
        helper_Increase_Flow_Rate_Allowance(superToken, bob, flowRate);

        vm.stopPrank();

        vm.prank(bob);
        superToken.createFlowFrom(alice, bob, flowRate);

        assertEq(superToken.getFlowRate(alice, bob), flowRate);
    }

    function test_Revert_If_Decrease_Flow_Rate_Allowance_And_ACL_Create_Flow(
        int96 flowRateAllowanceIncreaseDelta
    ) public {
        vm.assume(flowRateAllowanceIncreaseDelta > 0);

        vm.startPrank(alice);
        superToken.setFlowPermissions(bob, true, true, true, 0);
        helper_Increase_Flow_Rate_Allowance(
            superToken,
            bob,
            flowRateAllowanceIncreaseDelta
        );
        helper_Decrease_Flow_Rate_Allowance(
            superToken,
            bob,
            flowRateAllowanceIncreaseDelta
        );

        vm.stopPrank();

        vm.prank(bob);
        vm.expectRevert(
            IConstantFlowAgreementV1
                .CFA_ACL_FLOW_RATE_ALLOWANCE_EXCEEDED
                .selector
        );
        superToken.createFlowFrom(alice, bob, flowRateAllowanceIncreaseDelta);
    }

    function test_Revert_If_Increase_Flow_Rate_Allowance_Overflows() public {
        vm.startPrank(alice);
        helper_Increase_Flow_Rate_Allowance(
            superToken,
            bob,
            type(int96).max
        );
        vm.expectRevert("CallUtils: target panicked: 0x11");
        helper_Increase_Flow_Rate_Allowance(
            superToken,
            bob,
            1
        );
        vm.stopPrank();
    }

    function test_Revert_If_Decrease_Flow_Rate_Allowance_Underflows() public {
        vm.startPrank(alice);
        vm.expectRevert("CallUtils: target panicked: 0x01");
        helper_Decrease_Flow_Rate_Allowance(
            superToken,
            bob,
            1
        );
        vm.stopPrank();
    }
}
