// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

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
    function _startStream(address sender, address receiver, int96 flowRate) internal {
        vm.startPrank(sender);
        sf.cfaLib.createFlow(receiver, superToken, flowRate);
        vm.stopPrank();
    }

    function _transferAllToSink(address sender) internal {
        vm.startPrank(sender);
        superToken.transferAll(admin);
        vm.stopPrank();
    }

    function _assertNoFlow(address sender, address receiver) internal {
        (,int96 flow,,) = sf.cfaLib.cfa.getFlow(superToken, alice, bob);
        assertEq(flow, 0);
    }

    function testSingleLiquidation() public {

        _startStream(alice, bob, FLOW_RATE);
        _transferAllToSink(alice);

        vm.startPrank(liquidator);
        uint256 balance = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        batchLiquidator.deleteFlow(address(superToken), alice, bob);
        _assertNoFlow(alice, bob);

        assertTrue(superToken.balanceOf(liquidator) > balance);
        vm.stopPrank();
    }

    function testRevertIfArrayLengthsDontMatch() public {
        address[] memory senders = new address[](8);
        address[] memory receivers = new address[](7);
        vm.expectRevert(BatchLiquidator.ARRAY_SIZES_DIFFERENT.selector);
        batchLiquidator.deleteFlows(address(superToken), senders, receivers);
    }

    function testBatchLiquidation() public {

        _startStream(alice, bob, FLOW_RATE);
        _startStream(carol, bob, FLOW_RATE);
        _startStream(dan, bob, FLOW_RATE);

        _transferAllToSink(alice);
        _transferAllToSink(carol);
        _transferAllToSink(dan);


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
        _assertNoFlow(alice, bob);
        _assertNoFlow(carol, bob);
        _assertNoFlow(dan, bob);

        uint256 balanceAfter = superToken.balanceOf(liquidator);
        assertTrue(superToken.balanceOf(liquidator) > balance);
        vm.stopPrank();
    }

    function testBatchLiquidationWithRevert() public {

        _startStream(alice, bob, FLOW_RATE);
        _startStream(dan, bob, FLOW_RATE);

        _transferAllToSink(alice);
        _transferAllToSink(dan);

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
        _assertNoFlow(alice, bob);
        _assertNoFlow(carol, bob);
        _assertNoFlow(dan, bob);

        uint256 balanceAfter = superToken.balanceOf(liquidator);
        assertTrue(superToken.balanceOf(liquidator) > balance);
        vm.stopPrank();
    }
}
