// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { IConstantFlowAgreementV1 } from "../../../contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ISuperfluidToken } from "../../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";

contract ConstantFlowAgreementV1ACLTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;

    struct AssertFlowOperator {
        ISuperfluidToken superToken;
        bytes32 flowOperatorId;
        uint8 expectedPermissions;
        int96 expectedFlowRateAllowance;
    }

    constructor() FoundrySuperfluidTester(3) { }

    function testIncreaseFlowRateAllowance(int96 flowRateAllowanceDelta) public {
        vm.assume(flowRateAllowanceDelta > 0);

        bytes32 flowOperatorId = _generate_Flow_Operator_Id(alice, bob);
        (uint8 oldPermissions, int96 oldFlowRateAllowance) = sf.cfa.getFlowOperatorDataByID(superToken, flowOperatorId);

        vm.startPrank(alice);
        _increase_Flow_Rate_Allowance(superToken, bob, flowRateAllowanceDelta);
        vm.stopPrank();

        _assertFlowOperatorData(
            AssertFlowOperator({
                superToken: superToken,
                flowOperatorId: flowOperatorId,
                expectedPermissions: oldPermissions,
                expectedFlowRateAllowance: oldFlowRateAllowance + flowRateAllowanceDelta
            })
        );
    }

    function testDecreaseFlowRateAllowance(int96 flowRateAllowanceIncreaseDelta, int96 flowRateAllowanceDecreaseDelta)
        public
    {
        vm.assume(flowRateAllowanceIncreaseDelta > 0);
        vm.assume(flowRateAllowanceDecreaseDelta > 0);
        vm.assume(flowRateAllowanceDecreaseDelta <= flowRateAllowanceIncreaseDelta);

        (bytes32 oldFlowOperatorId, uint8 oldPermissions, int96 oldFlowRateAllowance) =
            sf.cfa.getFlowOperatorData(superToken, alice, bob);

        vm.startPrank(alice);
        _increase_Flow_Rate_Allowance(superToken, bob, flowRateAllowanceIncreaseDelta);
        _decrease_Flow_Rate_Allowance(superToken, bob, flowRateAllowanceDecreaseDelta);
        vm.stopPrank();

        _assertFlowOperatorData(
            AssertFlowOperator({
                superToken: superToken,
                flowOperatorId: oldFlowOperatorId,
                expectedPermissions: oldPermissions,
                expectedFlowRateAllowance: oldFlowRateAllowance + flowRateAllowanceIncreaseDelta
                    - flowRateAllowanceDecreaseDelta
            })
        );
    }

    function testIncreaseFlowRateAllowanceWithPreSetPermissions(int96 flowRateAllowanceDelta) public {
        vm.assume(flowRateAllowanceDelta > 0);

        vm.startPrank(alice);
        superToken.setFlowPermissions(bob, true, true, true, 0);

        (bytes32 oldFlowOperatorId, uint8 oldPermissions, int96 oldFlowRateAllowance) =
            sf.cfa.getFlowOperatorData(superToken, alice, bob);

        _increase_Flow_Rate_Allowance(superToken, bob, flowRateAllowanceDelta);
        vm.stopPrank();

        _assertFlowOperatorData(
            AssertFlowOperator({
                superToken: superToken,
                flowOperatorId: oldFlowOperatorId,
                expectedPermissions: oldPermissions,
                expectedFlowRateAllowance: oldFlowRateAllowance + flowRateAllowanceDelta
            })
        );
    }

    function testDecreaseFlowRateAllowanceWithPreSetPermissions(
        int96 flowRateAllowanceIncreaseDelta,
        int96 flowRateAllowanceDecreaseDelta
    ) public {
        vm.assume(flowRateAllowanceIncreaseDelta > 0);
        vm.assume(flowRateAllowanceDecreaseDelta > 0);
        vm.assume(flowRateAllowanceDecreaseDelta <= flowRateAllowanceIncreaseDelta);

        vm.startPrank(alice);
        superToken.setFlowPermissions(bob, true, true, true, 0);

        (bytes32 oldFlowOperatorId, uint8 oldPermissions, int96 oldFlowRateAllowance) =
            sf.cfa.getFlowOperatorData(superToken, alice, bob);

        _increase_Flow_Rate_Allowance(superToken, bob, flowRateAllowanceIncreaseDelta);
        _decrease_Flow_Rate_Allowance(superToken, bob, flowRateAllowanceDecreaseDelta);
        vm.stopPrank();

        _assertFlowOperatorData(
            AssertFlowOperator({
                superToken: superToken,
                flowOperatorId: oldFlowOperatorId,
                expectedPermissions: oldPermissions,
                expectedFlowRateAllowance: oldFlowRateAllowance + flowRateAllowanceIncreaseDelta
                    - flowRateAllowanceDecreaseDelta
            })
        );
    }

    function testIncreaseFlowRateAllowanceAndACLCreateFlow(uint32 flowRateAllowanceDelta) public {
        vm.assume(flowRateAllowanceDelta > 0);
        vm.assume(flowRateAllowanceDelta <= uint32(type(int32).max));
        int96 flowRate = int96(int32(flowRateAllowanceDelta));
        vm.assume(flowRateAllowanceDelta > 0);

        vm.startPrank(alice);
        superToken.setFlowPermissions(bob, true, true, true, 0);
        _increase_Flow_Rate_Allowance(superToken, bob, flowRate);

        vm.stopPrank();

        vm.prank(bob);
        superToken.createFlowFrom(alice, bob, flowRate);

        assertEq(superToken.getFlowRate(alice, bob), flowRate);
    }

    function testRevertIfDecreaseFlowRateAllowanceAndACLCreateFlow(int96 flowRateAllowanceIncreaseDelta) public {
        vm.assume(flowRateAllowanceIncreaseDelta > 0);

        vm.startPrank(alice);
        superToken.setFlowPermissions(bob, true, true, true, 0);
        _increase_Flow_Rate_Allowance(superToken, bob, flowRateAllowanceIncreaseDelta);
        _decrease_Flow_Rate_Allowance(superToken, bob, flowRateAllowanceIncreaseDelta);

        vm.stopPrank();

        vm.prank(bob);
        vm.expectRevert(IConstantFlowAgreementV1.CFA_ACL_FLOW_RATE_ALLOWANCE_EXCEEDED.selector);
        superToken.createFlowFrom(alice, bob, flowRateAllowanceIncreaseDelta);
    }

    function testRevertIfIncreaseFlowRateAllowanceOverflows() public {
        vm.startPrank(alice);
        _increase_Flow_Rate_Allowance(superToken, bob, type(int96).max);
        vm.expectRevert("CallUtils: target panicked: 0x11");
        _increase_Flow_Rate_Allowance(superToken, bob, 1);
        vm.stopPrank();
    }

    function testRevertIfDecreaseFlowRateAllowanceUnderflows() public {
        vm.startPrank(alice);
        vm.expectRevert(IConstantFlowAgreementV1.CFA_ACL_NO_NEGATIVE_ALLOWANCE.selector);
        _decrease_Flow_Rate_Allowance(superToken, bob, 10);
        vm.stopPrank();
    }

    function _assertFlowOperatorData(AssertFlowOperator memory data) internal {
        (uint8 newPermissions, int96 newFlowRateAllowance) =
            sf.cfa.getFlowOperatorDataByID(data.superToken, data.flowOperatorId);

        assertEq(newPermissions, data.expectedPermissions, "CFAv1 ACL: permissions not equal");
        assertEq(newFlowRateAllowance, data.expectedFlowRateAllowance, "CFAv1 ACL: flow rate allowance not equal");
    }

    function _generate_Flow_Operator_Id(address sender, address flowOperator) private pure returns (bytes32 id) {
        return keccak256(abi.encode("flowOperator", sender, flowOperator));
    }

    function _increase_Flow_Rate_Allowance(
        ISuperfluidToken _superToken,
        address flowOperator,
        int96 flowRateAllowanceDelta
    ) internal {
        // TODO: move these to SuperTokenV1Library
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(
                sf.cfa.increaseFlowRateAllowance, (_superToken, flowOperator, flowRateAllowanceDelta, new bytes(0))
            ),
            new bytes(0)
        );
    }

    function _decrease_Flow_Rate_Allowance(
        ISuperfluidToken _superToken,
        address flowOperator,
        int96 flowRateAllowanceDelta
    ) internal {
        // TODO: move these to SuperTokenV1Library
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(
                sf.cfa.decreaseFlowRateAllowance, (_superToken, flowOperator, flowRateAllowanceDelta, new bytes(0))
            ),
            new bytes(0)
        );
    }
}
