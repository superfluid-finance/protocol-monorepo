// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {stdError} from "forge-std/Test.sol";

import {
    BatchOperation,
    ISuperfluid,
    Superfluid
} from "../../../contracts/superfluid/Superfluid.sol";
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

contract SuperfluidBatchCallTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;

    constructor() FoundrySuperfluidTester(3) {}

    function test_Fuzz_Revert_If_Operation_Increase_Allowance_Is_Not_Called_By_Host(
        address notHost
    ) public {
        vm.assume(notHost != address(sf.host));

        vm.expectRevert(ISuperfluidToken.SF_TOKEN_ONLY_HOST.selector);
        vm.prank(notHost);
        superToken.operationIncreaseAllowance(notHost, alice, 100);
    }

    function test_Fuzz_Revert_If_Operation_Decrease_Allowance_Is_Not_Called_By_Host(
        address notHost
    ) public {
        vm.assume(notHost != address(sf.host));

        vm.expectRevert(ISuperfluidToken.SF_TOKEN_ONLY_HOST.selector);
        vm.prank(notHost);
        superToken.operationDecreaseAllowance(notHost, alice, 100);
    }

    function test_Revert_If_Operation_Decrease_Allowance_Underflows() public {
        vm.expectRevert("SuperToken: decreased allowance below zero");
        vm.prank(address(sf.host));
        superToken.operationDecreaseAllowance(alice, bob, 1);
    }

    function test_Revert_If_Operation_Increase_Allowance_Overflows() public {
        vm.startPrank(address(sf.host));
        superToken.operationIncreaseAllowance(alice, bob, type(uint256).max);
        vm.expectRevert(stdError.arithmeticError);
        superToken.operationIncreaseAllowance(alice, bob, 1);
        vm.stopPrank();
    }

    function test_Passing_If_Operation_Increase_Allowance_Is_Called_By_Host()
        public
    {
        uint256 aliceToBobAllowanceBefore = superToken.allowance(alice, bob);

        vm.prank(address(sf.host));
        superToken.operationIncreaseAllowance(alice, bob, 100);
        uint256 aliceToBobAllowanceAfter = superToken.allowance(alice, bob);

        assertEq(aliceToBobAllowanceAfter, aliceToBobAllowanceBefore + 100);
    }

    function test_Passing_If_Operation_Decrease_Allowance_Is_Called_By_Host()
        public
    {
        uint256 aliceToBobAllowanceBefore = superToken.allowance(alice, bob);

        vm.startPrank(address(sf.host));
        superToken.operationIncreaseAllowance(alice, bob, 100);
        superToken.operationDecreaseAllowance(alice, bob, 31);
        vm.stopPrank();

        uint256 aliceToBobAllowanceAfter = superToken.allowance(alice, bob);
        assertEq(aliceToBobAllowanceAfter, aliceToBobAllowanceBefore + 69);
    }

    function test_Passing_Increase_Allowance_Batch_Call(
        uint256 allowanceAmount
    ) public {
        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](1);
        uint256 aliceToBobAllowanceBefore = superToken.allowance(alice, bob);
        ops[0] = ISuperfluid.Operation({
            operationType: BatchOperation
                .OPERATION_TYPE_ERC20_INCREASE_ALLOWANCE,
            target: address(superToken),
            data: abi.encode(bob, allowanceAmount)
        });
        vm.prank(alice);
        sf.host.batchCall(ops);
        uint256 aliceToBobAllowanceAfter = superToken.allowance(alice, bob);
        assertEq(
            aliceToBobAllowanceAfter,
            aliceToBobAllowanceBefore + allowanceAmount
        );
    }

    function test_Passing_Decrease_Allowance_Batch_Call(
        uint256 increaseAllowanceAmount,
        uint256 decreaseAllowanceAmount
    ) public {
        vm.assume(increaseAllowanceAmount >= decreaseAllowanceAmount);

        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](1);
        vm.prank(alice);
        superToken.increaseAllowance(bob, increaseAllowanceAmount);
        uint256 aliceToBobAllowanceBefore = superToken.allowance(alice, bob);
        ops[0] = ISuperfluid.Operation({
            operationType: BatchOperation
                .OPERATION_TYPE_ERC20_DECREASE_ALLOWANCE,
            target: address(superToken),
            data: abi.encode(bob, decreaseAllowanceAmount)
        });
        vm.prank(alice);
        sf.host.batchCall(ops);
        uint256 aliceToBobAllowanceAfter = superToken.allowance(alice, bob);
        assertEq(
            aliceToBobAllowanceAfter,
            aliceToBobAllowanceBefore - decreaseAllowanceAmount
        );
    }

    function test_Passing_Increase_Decrease_Transfer_From_Batch_Call() public {
        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](2);
        uint256 aliceToBobAllowanceBefore = superToken.allowance(alice, bob);
        uint256 bobBalanceBefore = superToken.balanceOf(bob);
        ops[0] = ISuperfluid.Operation({
            operationType: BatchOperation
                .OPERATION_TYPE_ERC20_INCREASE_ALLOWANCE,
            target: address(superToken),
            data: abi.encode(bob, 100)
        });
        ops[1] = ISuperfluid.Operation({
            operationType: BatchOperation
                .OPERATION_TYPE_ERC20_DECREASE_ALLOWANCE,
            target: address(superToken),
            data: abi.encode(bob, 50)
        });

        vm.prank(alice);
        sf.host.batchCall(ops);
        uint256 aliceToBobAllowanceAfter = superToken.allowance(alice, bob);
        assertEq(aliceToBobAllowanceAfter, aliceToBobAllowanceBefore + 50);

        vm.prank(bob);
        superToken.transferFrom(alice, bob, 50);
        uint256 bobBalanceAfter = superToken.balanceOf(bob);
        aliceToBobAllowanceAfter = superToken.allowance(alice, bob);
        assertEq(aliceToBobAllowanceAfter, 0);
        assertEq(bobBalanceAfter, bobBalanceBefore + 50);
    }

    function test_Passing_Increase_Transfer_Allowance_And_Increase_Flow_Rate_Allowance()
        public
    {
        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](2);
        uint256 aliceToBobAllowanceBefore = superToken.allowance(alice, bob);

        (, , int96 flowRateAllowanceBefore) = sf.cfa.getFlowOperatorData(
            superToken,
            alice,
            bob
        );

        ops[0] = ISuperfluid.Operation({
            operationType: BatchOperation
                .OPERATION_TYPE_ERC20_INCREASE_ALLOWANCE,
            target: address(superToken),
            data: abi.encode(bob, 100)
        });
        bytes memory increaseFlowRateAllowanceCallData = abi.encodeCall(
            IConstantFlowAgreementV1.increaseFlowRateAllowance,
            (superToken, bob, 100, new bytes(0))
        );
        ops[1] = ISuperfluid.Operation({
            operationType: BatchOperation
                .OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT,
            target: address(sf.cfa),
            data: abi.encode(increaseFlowRateAllowanceCallData, new bytes(0))
        });

        vm.prank(alice);
        sf.host.batchCall(ops);

        uint256 aliceToBobAllowanceAfter = superToken.allowance(alice, bob);
        (, , int96 flowRateAllowanceAfter) = sf.cfa.getFlowOperatorData(
            superToken,
            alice,
            bob
        );
        assertEq(aliceToBobAllowanceAfter, aliceToBobAllowanceBefore + 100);
        assertEq(flowRateAllowanceAfter, flowRateAllowanceBefore + 100);
    }
}
