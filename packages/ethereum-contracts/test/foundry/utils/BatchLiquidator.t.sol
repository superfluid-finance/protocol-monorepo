// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";
import { ISuperToken, SuperToken, ISuperfluid, IConstantOutflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/SuperToken.sol";
import { BatchLiquidator } from "../../../contracts/utils/BatchLiquidator.sol";
import "forge-std/Test.sol";

contract NonTransferableST is SuperToken {
    // transferFrom will always revert
    constructor(
        ISuperfluid host
    )
    SuperToken(host, IConstantOutflowNFT(address(0)), IConstantInflowNFT(address(0))) // solhint-disable-next-line no-empty-blocks
    {
    }

    function transferFrom(address holder, address recipient, uint256 amount)  public override returns (bool) {
        revert();
    }

    function mintInternal(
        address to,
        uint256 amount
    ) external {
        _mint(msg.sender, to, amount, false /* invokeHook */, false /* requireReceptionAck */, "", "");
    }
}

contract BatchLiquidatorTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    BatchLiquidator internal batchLiquidator;
    address internal liquidator = address(0x1234);

    int96 internal constant FLOW_RATE = 10000000000;

    ISuperToken badToken;

    constructor() FoundrySuperfluidTester(5) { }

    function setUp() public override {
        super.setUp();
        batchLiquidator = new BatchLiquidator(address(sf.host), address(sf.cfa));
        badToken = new NonTransferableST(sf.host);
    }

    // Helpers
    function _startStream(address sender, address receiver, int96 flowRate) internal {
        vm.startPrank(sender);
        superToken.createFlow(receiver, flowRate);
        vm.stopPrank();
    }

    function _transferAllToSink(address sender) internal {
        vm.startPrank(sender);
        superToken.transferAll(admin);
        vm.stopPrank();
    }

    function _assertNoFlow(address sender, address receiver) internal {
        (, int96 flow,,) = sf.cfa.getFlow(superToken, sender, receiver);
        assertEq(flow, 0, "BatchLiquidator: Flow should be 0");
    }

    function testSingleLiquidation() public {
        _startStream(alice, bob, FLOW_RATE);
        _transferAllToSink(alice);

        vm.startPrank(liquidator);
        uint256 balance = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        batchLiquidator.deleteFlow(address(superToken), alice, bob);
        _assertNoFlow(alice, bob);

        assertTrue(
            superToken.balanceOf(liquidator) > balance, "BatchLiquidator: SL - Balance should be greater than before"
        );
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

        assertTrue(
            superToken.balanceOf(liquidator) > balance, "BatchLiquidator: BL - Balance should be greater than before"
        );
        vm.stopPrank();
    }

    function testBatchLiquidationWithToleratedRevert() public {
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

        assertTrue(
            superToken.balanceOf(liquidator) > balance, "BatchLiquidator: BLR - Balance should be greater than before"
        );
        vm.stopPrank();
    }

    function testLiquidationWithCustomTokenRevert() public {
        NonTransferableST(address(badToken)).mintInternal(alice, 10 ether);

        vm.startPrank(alice);
        badToken.createFlow(bob, FLOW_RATE);
        badToken.transferAll(admin);
        vm.warp(4 hours); // jump 4 hours
        vm.stopPrank();
        vm.startPrank(liquidator);

        batchLiquidator.deleteFlow(address(badToken), alice, bob);
        _assertNoFlow(alice, bob);

        assertTrue(
            superToken.balanceOf(liquidator) == 0, "BatchLiquidator: SL - Balance should be 0 because of revert"
        );
        vm.stopPrank();

    }

    function testBatchLiquidationWithCustomTokenRevert() public {
        NonTransferableST(address(badToken)).mintInternal(alice, 10 ether);
        NonTransferableST(address(badToken)).mintInternal(bob, 10 ether);

        vm.startPrank(alice);
        badToken.createFlow(bob, FLOW_RATE);
        badToken.transferAll(admin);
        vm.stopPrank();

        vm.startPrank(bob);
        badToken.createFlow(carol, FLOW_RATE);
        badToken.transferAll(admin);
        vm.stopPrank();

        vm.warp(4 hours); // jump 4 hours

        vm.startPrank(liquidator);

        address[] memory senders = new address[](2);
        address[] memory receivers = new address[](2);
        senders[0] = alice;
        senders[1] = bob;
        receivers[0] = bob;
        receivers[1] = carol;

        batchLiquidator.deleteFlows(address(superToken), senders, receivers);
        _assertNoFlow(alice, bob);
        _assertNoFlow(bob, carol);
    }
}
