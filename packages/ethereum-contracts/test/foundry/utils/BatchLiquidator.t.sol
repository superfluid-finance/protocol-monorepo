// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";
import { BatchLiquidator } from "@superfluid-finance/ethereum-contracts/contracts/utils/BatchLiquidator.sol";

contract BatchLiquidatorTest is FoundrySuperfluidTester {
    using CFAv1Library for CFAv1Library.InitData;

    BatchLiquidator batchLiquidator;
    address liquidator = address(0x1234);

    int96 FLOW_RATE = 10000000000;

    constructor () FoundrySuperfluidTester(5) { }

    function setUp() public override {
        super.setUp();
        batchLiquidator = new BatchLiquidator(address(sf.host), address(sf.cfa));
    }


    // Helpers
    function _start_Stream(address sender, address receiver, int96 flowRate) internal {
        vm.startPrank(sender);
        sf.cfaLib.createFlow(receiver, superToken, flowRate);
        vm.stopPrank();
    }

    function _transfer_All_To_Sink(address sender) internal {
        vm.startPrank(sender);
        superToken.transferAll(admin);
        vm.stopPrank();
    }

    function _assert_No_Flow(address sender, address receiver) internal {
        (,int96 flow,,) = sf.cfaLib.cfa.getFlow(superToken, alice, bob);
        assertEq(flow, 0, "BatchLiquidator: Flow should be 0");
    }

    function testSingleLiquidation() public {

        _start_Stream(alice, bob, FLOW_RATE);
        _transfer_All_To_Sink(alice);

        vm.startPrank(liquidator);
        uint256 balance = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        batchLiquidator.deleteFlow(address(superToken), alice, bob);
        _assert_No_Flow(alice, bob);

        assertTrue(superToken.balanceOf(liquidator) > balance, "BatchLiquidator: SL - Balance should be greater than before");
        vm.stopPrank();
    }

    function testSingleLiquidationRevert() public {
        vm.startPrank(liquidator);
        vm.expectRevert();
        batchLiquidator.deleteFlow(address(superToken), alice, bob);
        vm.stopPrank();
    }

    function testRevertIfArrayLengthsDontMatch() public {
        address[] memory senders = new address[](8);
        address[] memory receivers = new address[](7);
        vm.expectRevert(BatchLiquidator.ARRAY_SIZES_DIFFERENT.selector);
        batchLiquidator.deleteFlows(address(superToken), senders, receivers);
    }

    function testBatchLiquidation() public {

        _start_Stream(alice, bob, FLOW_RATE);
        _start_Stream(carol, bob, FLOW_RATE);
        _start_Stream(dan, bob, FLOW_RATE);

        _transfer_All_To_Sink(alice);
        _transfer_All_To_Sink(carol);
        _transfer_All_To_Sink(dan);


        vm.startPrank(liquidator);
        uint256 balance = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        address[] memory senders = new address[](3);
        address[] memory receivers = new address[](3);
        senders[0] = alice;
        senders[1] = carol;
        senders[2] = dan;
        receivers[0] = bob;
        receivers[1] = bob;
        receivers[2] = bob;

        batchLiquidator.deleteFlows(address(superToken), senders, receivers);
        _assert_No_Flow(alice, bob);
        _assert_No_Flow(carol, bob);
        _assert_No_Flow(dan, bob);

        uint256 balanceAfter = superToken.balanceOf(liquidator);
        assertTrue(superToken.balanceOf(liquidator) > balance, "BatchLiquidator: BL - Balance should be greater than before");
        vm.stopPrank();
    }

    function testBatchLiquidationWithToleratedRevert() public {

        _start_Stream(alice, bob, FLOW_RATE);
        _start_Stream(dan, bob, FLOW_RATE);

        _transfer_All_To_Sink(alice);
        _transfer_All_To_Sink(dan);

        vm.startPrank(liquidator);
        uint256 balance = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        address[] memory senders = new address[](3);
        address[] memory receivers = new address[](3);
        senders[0] = alice;
        senders[1] = carol; // carol has no flow
        senders[2] = dan;
        receivers[0] = bob;
        receivers[1] = bob;
        receivers[2] = bob;

        batchLiquidator.deleteFlows(address(superToken), senders, receivers);
        _assert_No_Flow(alice, bob);
        _assert_No_Flow(carol, bob);
        _assert_No_Flow(dan, bob);

        uint256 balanceAfter = superToken.balanceOf(liquidator);
        assertTrue(superToken.balanceOf(liquidator) > balance, "BatchLiquidator: BLR - Balance should be greater than before");
        vm.stopPrank();
    }
}
