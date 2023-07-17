// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { ISuperfluidPool } from "../../../contracts/interfaces/superfluid/ISuperfluidPool.sol";
import { BatchLiquidator } from "../../../contracts/utils/BatchLiquidator.sol";

contract BatchLiquidatorTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    BatchLiquidator internal batchLiquidator;
    address internal liquidator = address(0x1234);

    int96 internal constant FLOW_RATE = 10000000000;

    constructor() FoundrySuperfluidTester(5) { }

    function setUp() public override {
        super.setUp();
        batchLiquidator = new BatchLiquidator(address(sf.host));
    }

    // Helpers

    function _transferAllToSink(address sender) internal {
        vm.startPrank(sender);
        superToken.transferAll(admin);
        vm.stopPrank();
    }

    function _assertNoCFAFlow(address sender, address receiver) internal {
        (, int96 flowRate,,) = sf.cfa.getFlow(superToken, sender, receiver);
        assertEq(flowRate, 0, "BatchLiquidator: CFA Flowrate should be 0");
    }

    function _assertNoGDAFlow(address sender, ISuperfluidPool pool) internal {
        int96 flowRate = sf.gda.getFlowRate(superToken, sender, pool);
        assertEq(flowRate, 0, "BatchLiquidator: GDA Flowrate should be 0");
    }

    function _assertLiquidatorBalanceGreater(address _liqudidator, uint256 balanceBefore_) internal {
        assertGt(
            superToken.balanceOf(_liqudidator),
            balanceBefore_,
            "BatchLiquidator: SL - Balance should be greater than before"
        );
    }

    function _createCFAFlowLiquidationData(address sender, address receiver)
        internal
        pure
        returns (BatchLiquidator.FlowLiquidationData memory)
    {
        return BatchLiquidator.FlowLiquidationData({
            agreementOperation: BatchLiquidator.FlowType.ConstantFlowAgreement,
            sender: sender,
            receiver: receiver
        });
    }

    function _createGDAFlowLiquidationData(address sender, ISuperfluidPool pool)
        internal
        pure
        returns (BatchLiquidator.FlowLiquidationData memory)
    {
        return BatchLiquidator.FlowLiquidationData({
            agreementOperation: BatchLiquidator.FlowType.GeneralDistributionAgreement,
            sender: sender,
            receiver: address(pool)
        });
    }

    function testCFAOnlySingleLiquidation() public {
        _helperCreateFlow(superToken, alice, bob, FLOW_RATE);
        _transferAllToSink(alice);

        vm.startPrank(liquidator);
        uint256 balanceBefore = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        batchLiquidator.deleteFlow(address(superToken), _createCFAFlowLiquidationData(alice, bob));
        _assertNoCFAFlow(alice, bob);

        _assertLiquidatorBalanceGreater(liquidator, balanceBefore);
        vm.stopPrank();
    }

    function testGDAOnlySingleLiquidation() public {
        ISuperfluidPool pool = _helperCreatePool(superToken, alice, alice);
        _helperUpdateMemberUnits(pool, alice, bob, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, FLOW_RATE);

        _transferAllToSink(alice);

        vm.startPrank(liquidator);
        uint256 balanceBefore = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        batchLiquidator.deleteFlow(address(superToken), _createGDAFlowLiquidationData(alice, pool));
        _assertNoGDAFlow(alice, pool);

        _assertLiquidatorBalanceGreater(liquidator, balanceBefore);
        vm.stopPrank();
    }

    function testCFAOnlySingleLiquidationRevert() public {
        vm.startPrank(liquidator);
        vm.expectRevert();
        batchLiquidator.deleteFlow(address(superToken), _createCFAFlowLiquidationData(alice, bob));
        vm.stopPrank();
    }

    function testGDAOnlySingleLiquidationRevert() public {
        ISuperfluidPool pool = _helperCreatePool(superToken, alice, alice);
        vm.startPrank(liquidator);
        vm.expectRevert();
        batchLiquidator.deleteFlow(address(superToken), _createGDAFlowLiquidationData(alice, pool));
        vm.stopPrank();
    }

    function testCFAOnlyBatchLiquidation() public {
        _helperCreateFlow(superToken, alice, bob, FLOW_RATE);
        _helperCreateFlow(superToken, carol, bob, FLOW_RATE);
        _helperCreateFlow(superToken, dan, bob, FLOW_RATE);

        _transferAllToSink(alice);
        _transferAllToSink(carol);
        _transferAllToSink(dan);

        vm.startPrank(liquidator);
        uint256 balanceBefore = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        BatchLiquidator.FlowLiquidationData[] memory data = new BatchLiquidator.FlowLiquidationData[](3);
        data[0] = _createCFAFlowLiquidationData(alice, bob);
        data[1] = _createCFAFlowLiquidationData(carol, bob);
        data[2] = _createCFAFlowLiquidationData(dan, bob);

        batchLiquidator.deleteFlows(address(superToken), data);

        _assertNoCFAFlow(alice, bob);
        _assertNoCFAFlow(carol, bob);
        _assertNoCFAFlow(dan, bob);

        _assertLiquidatorBalanceGreater(liquidator, balanceBefore);
        vm.stopPrank();
    }

    function testGDAOnlyBatchLiquidation() public {
        ISuperfluidPool pool = _helperCreatePool(superToken, alice, alice);
        _helperUpdateMemberUnits(pool, alice, bob, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, FLOW_RATE);
        _helperDistributeFlow(superToken, carol, carol, pool, FLOW_RATE);
        _helperDistributeFlow(superToken, dan, dan, pool, FLOW_RATE);

        int96 flowRate = sf.gda.getFlowRate(superToken, alice, pool);

        _transferAllToSink(alice);
        _transferAllToSink(carol);
        _transferAllToSink(dan);

        vm.startPrank(liquidator);
        uint256 balanceBefore = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        BatchLiquidator.FlowLiquidationData[] memory data = new BatchLiquidator.FlowLiquidationData[](3);
        data[0] = _createGDAFlowLiquidationData(alice, pool);
        data[1] = _createGDAFlowLiquidationData(carol, pool);
        data[2] = _createGDAFlowLiquidationData(dan, pool);
        batchLiquidator.deleteFlows(address(superToken), data);

        flowRate = sf.gda.getFlowRate(superToken, alice, pool);

        _assertNoGDAFlow(alice, pool);
        _assertNoGDAFlow(carol, pool);
        _assertNoGDAFlow(dan, pool);

        _assertLiquidatorBalanceGreater(liquidator, balanceBefore);
        vm.stopPrank();
    }

    function testCFAOnlyBatchLiquidationWithToleratedRevert() public {
        _helperCreateFlow(superToken, alice, bob, FLOW_RATE);
        _helperCreateFlow(superToken, dan, bob, FLOW_RATE);

        _transferAllToSink(alice);
        _transferAllToSink(dan);

        vm.startPrank(liquidator);
        uint256 balanceBefore = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        BatchLiquidator.FlowLiquidationData[] memory data = new BatchLiquidator.FlowLiquidationData[](3);

        data[0].agreementOperation = BatchLiquidator.FlowType.ConstantFlowAgreement;
        data[0].sender = alice;
        data[0].receiver = bob;

        data[1].agreementOperation = BatchLiquidator.FlowType.ConstantFlowAgreement;
        data[1].sender = carol;
        data[1].receiver = bob;

        data[2].agreementOperation = BatchLiquidator.FlowType.ConstantFlowAgreement;
        data[2].sender = dan;
        data[2].receiver = bob;

        batchLiquidator.deleteFlows(address(superToken), data);
        _assertNoCFAFlow(alice, bob);
        _assertNoCFAFlow(carol, bob);
        _assertNoCFAFlow(dan, bob);

        _assertLiquidatorBalanceGreater(liquidator, balanceBefore);
        vm.stopPrank();
    }

    function testGDAOnlyBatchLiquidationWithToleratedRevert() public {
        ISuperfluidPool pool = _helperCreatePool(superToken, alice, alice);
        _helperUpdateMemberUnits(pool, alice, bob, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, FLOW_RATE);
        _helperDistributeFlow(superToken, dan, dan, pool, FLOW_RATE);

        int96 flowRate = sf.gda.getFlowRate(superToken, alice, pool);

        _transferAllToSink(alice);
        _transferAllToSink(dan);

        vm.startPrank(liquidator);
        uint256 balanceBefore = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        BatchLiquidator.FlowLiquidationData[] memory data = new BatchLiquidator.FlowLiquidationData[](3);
        data[0].agreementOperation = BatchLiquidator.FlowType.GeneralDistributionAgreement;
        data[0].sender = alice;
        data[0].receiver = address(pool);

        data[1].agreementOperation = BatchLiquidator.FlowType.GeneralDistributionAgreement;
        data[1].sender = carol;
        data[1].receiver = address(pool);

        data[2].agreementOperation = BatchLiquidator.FlowType.GeneralDistributionAgreement;
        data[2].sender = dan;
        data[2].receiver = address(pool);

        batchLiquidator.deleteFlows(address(superToken), data);

        flowRate = sf.gda.getFlowRate(superToken, alice, pool);

        _assertNoGDAFlow(alice, pool);
        _assertNoGDAFlow(carol, pool);
        _assertNoGDAFlow(dan, pool);

        _assertLiquidatorBalanceGreater(liquidator, balanceBefore);
        vm.stopPrank();
    }

    function testBatchLiquidation() public {
        ISuperfluidPool pool = _helperCreatePool(superToken, alice, alice);
        _helperUpdateMemberUnits(pool, alice, bob, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, FLOW_RATE);

        _helperCreateFlow(superToken, carol, bob, FLOW_RATE);
        _helperCreateFlow(superToken, dan, bob, FLOW_RATE);

        _transferAllToSink(alice);
        _transferAllToSink(carol);
        _transferAllToSink(dan);

        vm.startPrank(liquidator);
        uint256 balanceBefore = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        BatchLiquidator.FlowLiquidationData[] memory data = new BatchLiquidator.FlowLiquidationData[](4);

        data[0].agreementOperation = BatchLiquidator.FlowType.GeneralDistributionAgreement;
        data[0].sender = alice;
        data[0].receiver = address(pool);

        data[1].agreementOperation = BatchLiquidator.FlowType.ConstantFlowAgreement;
        data[1].sender = carol;
        data[1].receiver = bob;

        data[2].agreementOperation = BatchLiquidator.FlowType.ConstantFlowAgreement;
        data[2].sender = dan;
        data[2].receiver = bob;

        batchLiquidator.deleteFlows(address(superToken), data);

        _assertNoGDAFlow(alice, pool);
        _assertNoCFAFlow(carol, bob);
        _assertNoCFAFlow(dan, bob);

        _assertLiquidatorBalanceGreater(liquidator, balanceBefore);
        vm.stopPrank();
    }

    function testBatchLiquidationWithToleratedRevert() public {
        ISuperfluidPool pool = _helperCreatePool(superToken, alice, alice);
        _helperUpdateMemberUnits(pool, alice, bob, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, FLOW_RATE);

        _helperCreateFlow(superToken, dan, bob, FLOW_RATE);

        _transferAllToSink(alice);
        _transferAllToSink(dan);

        vm.startPrank(liquidator);
        uint256 balanceBefore = superToken.balanceOf(liquidator);
        vm.warp(4 hours); // jump 4 hours
        BatchLiquidator.FlowLiquidationData[] memory data = new BatchLiquidator.FlowLiquidationData[](4);

        data[0].agreementOperation = BatchLiquidator.FlowType.GeneralDistributionAgreement;
        data[0].sender = alice;
        data[0].receiver = address(pool);

        data[1].agreementOperation = BatchLiquidator.FlowType.ConstantFlowAgreement;
        data[1].sender = carol;
        data[1].receiver = bob;

        data[2].agreementOperation = BatchLiquidator.FlowType.ConstantFlowAgreement;
        data[2].sender = dan;
        data[2].receiver = bob;

        batchLiquidator.deleteFlows(address(superToken), data);

        _assertNoGDAFlow(alice, pool);
        _assertNoCFAFlow(carol, bob);
        _assertNoCFAFlow(dan, bob);

        _assertLiquidatorBalanceGreater(liquidator, balanceBefore);
        vm.stopPrank();
    }
}
