// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";
import { FlowOperatorDefinitions } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { IVestingSchedulerV2 } from "./../contracts/interface/IVestingSchedulerV2.sol";
import { VestingSchedulerV2 } from "./../contracts/VestingSchedulerV2.sol";
import { FoundrySuperfluidTester } from "@superfluid-finance/ethereum-contracts/test/foundry/FoundrySuperfluidTester.sol";
import { SuperTokenV1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperTokenV1Library.sol";
import { SafeMath } from "@openzeppelin/contracts/utils/math/SafeMath.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import "forge-std/console.sol";

/// @title VestingSchedulerTests
/// @notice Look at me , I am the captain now - Elvijs
contract VestingSchedulerV2Tests is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    event VestingScheduleCreated(
        ISuperToken indexed superToken,
        address indexed sender,
        address indexed receiver,
        uint32 startDate,
        uint32 cliffDate,
        int96 flowRate,
        uint32 endDate,
        uint256 cliffAmount,
        uint256 remainderAmount
    );

    event VestingScheduleUpdated(
        ISuperToken indexed superToken,
        address indexed sender,
        address indexed receiver,
        uint32 oldEndDate,
        uint32 endDate
    );

    event VestingScheduleDeleted(
        ISuperToken indexed superToken,
        address indexed sender,
        address indexed receiver
    );

    event VestingCliffAndFlowExecuted(
        ISuperToken indexed superToken,
        address indexed sender,
        address indexed receiver,
        uint32 cliffAndFlowDate,
        int96 flowRate,
        uint256 cliffAmount,
        uint256 flowDelayCompensation
    );

    event VestingEndExecuted(
        ISuperToken indexed superToken,
        address indexed sender,
        address indexed receiver,
        uint32 endDate,
        uint256 earlyEndCompensation,
        bool didCompensationFail
    );

    event VestingEndFailed(
        ISuperToken indexed superToken,
        address indexed sender,
        address indexed receiver,
        uint32 endDate
    );

    event Transfer(address indexed from, address indexed to, uint256 value);

    /// @dev This is required by solidity for using the SuperTokenV1Library in the tester
    VestingSchedulerV2 public vestingScheduler;

    /// @dev Constants for Testing
    uint256 immutable BLOCK_TIMESTAMP = 100;
    uint32 immutable START_DATE = uint32(BLOCK_TIMESTAMP + 1);
    uint32 immutable CLIFF_DATE = uint32(BLOCK_TIMESTAMP + 10 days);
    int96 constant FLOW_RATE = 1000000000;
    uint256 constant CLIFF_TRANSFER_AMOUNT = 1 ether;
    uint32 immutable END_DATE = uint32(BLOCK_TIMESTAMP + 20 days);
    bytes constant EMPTY_CTX = "";
    uint256 internal _expectedTotalSupply = 0;

    constructor() FoundrySuperfluidTester(3) {
        vestingScheduler = new VestingSchedulerV2(sf.host);
    }

    /// SETUP AND HELPERS
    function setUp() override public virtual {
        super.setUp();
        vm.warp(BLOCK_TIMESTAMP);
    }

    function _setACL_AUTHORIZE_FULL_CONTROL(address user, int96 flowRate) private {
        vm.startPrank(user);
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(
                sf.cfa.updateFlowOperatorPermissions,
                (
                superToken,
                address(vestingScheduler),
                FlowOperatorDefinitions.AUTHORIZE_FULL_CONTROL,
                flowRate,
                new bytes(0)
                )
            ),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function _arrangeAllowances(address sender, int96 flowRate) private {
        // ## Superfluid ACL allowance and permissions
        _setACL_AUTHORIZE_FULL_CONTROL(sender, flowRate);

        // ## ERC-20 allowance for cliff and compensation transfers
        vm.startPrank(sender);
        superToken.approve(address(vestingScheduler), type(uint256).max);
        vm.stopPrank();
    }

    function _createVestingScheduleWithDefaultData(address sender, address receiver) private {
        vm.startPrank(sender);
        vestingScheduler.createVestingSchedule(
            superToken,
            receiver,
            START_DATE,
            CLIFF_DATE,
            FLOW_RATE,
            CLIFF_TRANSFER_AMOUNT,
            END_DATE,
            EMPTY_CTX
        );
        vm.stopPrank();
    }

    /// TESTS

    function testCreateVestingSchedule() public {
        vm.expectEmit(true, true, true, true);
        emit VestingScheduleCreated(
            superToken, alice, bob, START_DATE, CLIFF_DATE, FLOW_RATE, END_DATE, CLIFF_TRANSFER_AMOUNT, 0
        );
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.startPrank(alice);
        //assert storage data
        VestingSchedulerV2.VestingSchedule memory schedule = vestingScheduler.getVestingSchedule(address(superToken), alice, bob);
        assertTrue(schedule.cliffAndFlowDate == CLIFF_DATE , "schedule.cliffAndFlowDate");
        assertTrue(schedule.endDate == END_DATE , "schedule.endDate");
        assertTrue(schedule.flowRate == FLOW_RATE , "schedule.flowRate");
        assertTrue(schedule.cliffAmount == CLIFF_TRANSFER_AMOUNT , "schedule.cliffAmount");
    }

    function testCannotCreateVestingScheduleWithWrongData() public {
        vm.startPrank(alice);
        // revert with superToken = 0
        vm.expectRevert(IVestingSchedulerV2.ZeroAddress.selector);
        vestingScheduler.createVestingSchedule(
                ISuperToken(address(0)),
                bob,
                START_DATE,
                CLIFF_DATE,
                FLOW_RATE,
                CLIFF_TRANSFER_AMOUNT,
                END_DATE,
                EMPTY_CTX
        );

        // revert with receivers = sender
        vm.expectRevert(IVestingSchedulerV2.AccountInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                alice,
                START_DATE,
                CLIFF_DATE,
                FLOW_RATE,
                CLIFF_TRANSFER_AMOUNT,
                END_DATE,
                EMPTY_CTX
        );

        // revert with receivers = address(0)
        vm.expectRevert(IVestingSchedulerV2.AccountInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                address(0),
                START_DATE,
                CLIFF_DATE,
                FLOW_RATE,
                CLIFF_TRANSFER_AMOUNT,
                END_DATE,
                EMPTY_CTX
        );

        // revert with flowRate = 0
        vm.expectRevert(IVestingSchedulerV2.FlowRateInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                START_DATE,
                CLIFF_DATE,
                0,
                CLIFF_TRANSFER_AMOUNT,
                END_DATE,
                EMPTY_CTX
        );

        // revert with cliffDate = 0 but cliffAmount != 0
        vm.expectRevert(IVestingSchedulerV2.CliffInvalid.selector);
        vestingScheduler.createVestingSchedule(
            superToken,
            bob,
            0,
            0,
            FLOW_RATE,
            CLIFF_TRANSFER_AMOUNT,
            END_DATE,
            EMPTY_CTX
        );

        // revert with startDate < block.timestamp && cliffDate = 0
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                uint32(block.timestamp - 1),
                0,
                FLOW_RATE,
                0,
                END_DATE,
                EMPTY_CTX
        );

        // revert with endDate = 0
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                START_DATE,
                CLIFF_DATE,
                FLOW_RATE,
                CLIFF_TRANSFER_AMOUNT,
                0,
                EMPTY_CTX
        );

        // revert with cliffAndFlowDate < block.timestamp
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                0,
                uint32(block.timestamp) - 1,
                FLOW_RATE,
                0,
                END_DATE,
                EMPTY_CTX
        );

        // revert with cliffAndFlowDate >= endDate
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                START_DATE,
                CLIFF_DATE,
                FLOW_RATE,
                CLIFF_TRANSFER_AMOUNT,
                CLIFF_DATE,
                EMPTY_CTX
        );

        // revert with cliffAndFlowDate + startDateValidFor >= endDate - endDateValidBefore
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                START_DATE,
                CLIFF_DATE,
                FLOW_RATE,
                CLIFF_TRANSFER_AMOUNT,
                CLIFF_DATE,
                EMPTY_CTX
        );

        // revert with startDate > cliffDate
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                CLIFF_DATE + 1,
                CLIFF_DATE,
                FLOW_RATE,
                CLIFF_TRANSFER_AMOUNT,
                END_DATE,
                EMPTY_CTX
        );


        // revert with vesting duration < 7 days
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                START_DATE,
                CLIFF_DATE,
                FLOW_RATE,
                CLIFF_TRANSFER_AMOUNT,
                CLIFF_DATE + 2 days,
                EMPTY_CTX
        );
    }

    function testCannotCreateVestingScheduleIfDataExist() public {
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.expectRevert(IVestingSchedulerV2.ScheduleAlreadyExists.selector);
        _createVestingScheduleWithDefaultData(alice, bob);
    }

    function testUpdateVestingSchedule() public {
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        vm.expectEmit(true, true, true, true);
        emit VestingScheduleCreated(
            superToken, alice, bob, START_DATE, CLIFF_DATE, FLOW_RATE, END_DATE, CLIFF_TRANSFER_AMOUNT, 0
        );
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.prank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.startPrank(admin);
        uint256 initialTimestamp = block.timestamp + 10 days + 1800;
        vm.warp(initialTimestamp);
        vestingScheduler.executeCliffAndFlow(superToken, alice, bob);
        vm.stopPrank();
        vm.startPrank(alice);
        vestingScheduler.updateVestingSchedule(superToken, bob, END_DATE + 1000, EMPTY_CTX);
        //assert storage data
        IVestingSchedulerV2.VestingSchedule memory schedule = vestingScheduler.getVestingSchedule(address(superToken), alice, bob);
        assertTrue(schedule.cliffAndFlowDate == 0 , "schedule.cliffAndFlowDate");
        assertTrue(schedule.endDate == END_DATE + 1000 , "schedule.endDate");
    }

    function testCannotUpdateVestingScheduleIfNotRunning() public {
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.startPrank(alice);
        vm.expectRevert(IVestingSchedulerV2.ScheduleNotFlowing.selector);
        vestingScheduler.updateVestingSchedule(superToken, bob, END_DATE, EMPTY_CTX);
    }

    function testCannotUpdateVestingScheduleIfDataDontExist() public {
        vm.startPrank(alice);
        vm.expectRevert(IVestingSchedulerV2.ScheduleNotFlowing.selector);
        vestingScheduler.updateVestingSchedule(superToken, bob, END_DATE, EMPTY_CTX);
    }

    function testDeleteVestingSchedule() public {
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.startPrank(alice);
        vm.expectEmit(true, true, true, true);
        emit VestingScheduleDeleted(superToken, alice, bob);
        vestingScheduler.deleteVestingSchedule(superToken, bob, EMPTY_CTX);
    }

    function testCannotDeleteVestingScheduleIfDataDontExist() public {
        vm.startPrank(alice);
        vm.expectRevert(IVestingSchedulerV2.ScheduleDoesNotExist.selector);
        vestingScheduler.deleteVestingSchedule(
            superToken,
            bob,
            EMPTY_CTX
        );
    }

    function testExecuteCliffAndFlowWithCliffAmount() public {
        uint256 aliceInitialBalance = superToken.balanceOf(alice);
        uint256 bobInitialBalance = superToken.balanceOf(bob);
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.prank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.startPrank(admin);
        uint256 initialTimestamp = block.timestamp + 10 days + 1800;
        vm.warp(initialTimestamp);
        uint256 flowDelayCompensation = (block.timestamp - CLIFF_DATE) * uint96(FLOW_RATE);
        vm.expectEmit(true, true, true, true);
        emit Transfer(alice, bob, CLIFF_TRANSFER_AMOUNT + flowDelayCompensation);
        vm.expectEmit(true, true, true, true);
        emit VestingCliffAndFlowExecuted(
            superToken, alice, bob, CLIFF_DATE, FLOW_RATE, CLIFF_TRANSFER_AMOUNT, flowDelayCompensation
        );
        bool success = vestingScheduler.executeCliffAndFlow(superToken, alice, bob);
        assertTrue(success, "executeVesting should return true");
        uint256 finalTimestamp = block.timestamp + 10 days - 3600;
        vm.warp(finalTimestamp);
        vm.expectEmit(true, true, true, true);
        uint256 timeDiffToEndDate = END_DATE > block.timestamp ? END_DATE - block.timestamp : 0;
        uint256 adjustedAmountClosing = timeDiffToEndDate * uint96(FLOW_RATE);
        emit Transfer(alice, bob, adjustedAmountClosing);
        vm.expectEmit(true, true, true, true);
        emit VestingEndExecuted(
            superToken, alice, bob, END_DATE, adjustedAmountClosing, false
        );
        success = vestingScheduler.executeEndVesting(superToken, alice, bob);
        assertTrue(success, "executeCloseVesting should return true");
        uint256 aliceFinalBalance = superToken.balanceOf(alice);
        uint256 bobFinalBalance = superToken.balanceOf(bob);
        uint256 aliceShouldStream = (END_DATE-CLIFF_DATE) * uint96(FLOW_RATE) + CLIFF_TRANSFER_AMOUNT ;
        assertEq(aliceInitialBalance - aliceFinalBalance, aliceShouldStream, "(sender) wrong final balance");
        assertEq(bobFinalBalance, bobInitialBalance + aliceShouldStream, "(receiver) wrong final balance");
    }

    function testExecuteCliffAndFlowWithoutCliffAmountOrAdjustment() public {
        uint256 aliceInitialBalance = superToken.balanceOf(alice);
        uint256 bobInitialBalance = superToken.balanceOf(bob);
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        vm.startPrank(alice);
        vestingScheduler.createVestingSchedule(
                superToken,
                bob,
                START_DATE,
                CLIFF_DATE,
                FLOW_RATE,
                0,
                END_DATE,
                EMPTY_CTX
        );
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.stopPrank();
        vm.startPrank(admin);
        vm.warp(CLIFF_DATE);
        vm.expectEmit(true, true, true, true);
        emit VestingCliffAndFlowExecuted(
            superToken, alice, bob, CLIFF_DATE, FLOW_RATE, 0, 0
        );
        bool success = vestingScheduler.executeCliffAndFlow(superToken, alice, bob);
        assertTrue(success, "executeVesting should return true");
        vm.warp(END_DATE);
        vm.expectEmit(true, true, true, true);
        emit VestingEndExecuted(superToken, alice, bob, END_DATE, 0, false);
        success = vestingScheduler.executeEndVesting(superToken, alice, bob);
        assertTrue(success, "executeCloseVesting should return true");
        uint256 aliceFinalBalance = superToken.balanceOf(alice);
        uint256 bobFinalBalance = superToken.balanceOf(bob);
        uint256 aliceShouldStream = (END_DATE-CLIFF_DATE) * uint96(FLOW_RATE);
        assertEq(aliceInitialBalance - aliceFinalBalance, aliceShouldStream, "(sender) wrong final balance");
        assertEq(bobFinalBalance, bobInitialBalance + aliceShouldStream, "(receiver) wrong final balance");
    }

    function testExecuteCliffAndFlowWithUpdatedEndDate() public {
        uint32 NEW_END_DATE = END_DATE - 1000;
        uint256 aliceInitialBalance = superToken.balanceOf(alice);
        uint256 bobInitialBalance = superToken.balanceOf(bob);
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.prank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.startPrank(admin);
        uint256 initialTimestamp = block.timestamp + 10 days + 1800;
        vm.warp(initialTimestamp);
        uint256 flowDelayCompensation = (block.timestamp - CLIFF_DATE) * uint96(FLOW_RATE);
        vm.expectEmit(true, true, true, true);
        emit Transfer(alice, bob, CLIFF_TRANSFER_AMOUNT + flowDelayCompensation);
        vm.expectEmit(true, true, true, true);
        emit VestingCliffAndFlowExecuted(
            superToken, alice, bob, CLIFF_DATE, FLOW_RATE, CLIFF_TRANSFER_AMOUNT, flowDelayCompensation
        );
        bool success = vestingScheduler.executeCliffAndFlow(superToken, alice, bob);
        assertTrue(success, "executeVesting should return true");
        vm.stopPrank();
        vm.prank(alice);
        vm.expectEmit(true, true, true, true);
        emit VestingScheduleUpdated(superToken, alice, bob, END_DATE, NEW_END_DATE);
        vestingScheduler.updateVestingSchedule(superToken, bob, NEW_END_DATE, EMPTY_CTX);
        uint256 finalTimestamp = block.timestamp + 10 days - 3600;
        vm.warp(finalTimestamp);
        vm.expectEmit(true, true, true, true);
        uint256 timeDiffToEndDate = NEW_END_DATE > block.timestamp ? NEW_END_DATE - block.timestamp : 0;
        uint256 adjustedAmountClosing = timeDiffToEndDate * uint96(FLOW_RATE);
        emit Transfer(alice, bob, adjustedAmountClosing);
        vm.expectEmit(true, true, true, true);
        emit VestingEndExecuted(
            superToken, alice, bob, NEW_END_DATE, adjustedAmountClosing, false
        );
        success = vestingScheduler.executeEndVesting(superToken, alice, bob);
        assertTrue(success, "executeCloseVesting should return true");
        uint256 aliceFinalBalance = superToken.balanceOf(alice);
        uint256 bobFinalBalance = superToken.balanceOf(bob);
        uint256 aliceShouldStream = (NEW_END_DATE-CLIFF_DATE) * uint96(FLOW_RATE) + CLIFF_TRANSFER_AMOUNT ;
        assertEq(aliceInitialBalance - aliceFinalBalance, aliceShouldStream, "(sender) wrong final balance");
        assertEq(bobFinalBalance, bobInitialBalance + aliceShouldStream, "(receiver) wrong final balance");
    }

    function testExecuteCliffAndFlowRevertClosingTransfer() public {
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.prank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.startPrank(admin);
        uint256 initialTimestamp = block.timestamp + 10 days + 1800;
        vm.warp(initialTimestamp);
        uint256 flowDelayCompensation = (block.timestamp - CLIFF_DATE) * uint96(FLOW_RATE);
        vm.expectEmit(true, true, true, true);
        emit Transfer(alice, bob, CLIFF_TRANSFER_AMOUNT + flowDelayCompensation);
        vm.expectEmit(true, true, true, true);
        emit VestingCliffAndFlowExecuted(
            superToken, alice, bob, CLIFF_DATE, FLOW_RATE, CLIFF_TRANSFER_AMOUNT, flowDelayCompensation
        );
        bool success = vestingScheduler.executeCliffAndFlow(superToken, alice, bob);
        assertTrue(success, "executeVesting should return true");
        vm.stopPrank();
        vm.startPrank(alice);
        superToken.transferAll(eve);
        vm.stopPrank();
        vm.startPrank(admin);
        uint256 earlyEndTimestamp = block.timestamp + 10 days - 3600;
        vm.warp(earlyEndTimestamp);

        vm.expectRevert();
        vestingScheduler.executeEndVesting(superToken, alice, bob);

        uint256 finalTimestamp = END_DATE + 1;
        vm.warp(finalTimestamp);

        vm.expectEmit(true, true, true, true);
        emit VestingEndExecuted(
            superToken, alice, bob, END_DATE, 0, true
        );
        success = vestingScheduler.executeEndVesting(superToken, alice, bob);
        assertTrue(success, "executeCloseVesting should return true");
    }

    function testCannotExecuteEndVestingBeforeTime() public {
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.prank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.startPrank(admin);
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.executeEndVesting(superToken, alice, bob);
    }

    function testCannotExecuteCliffAndFlowBeforeTime() public {
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.prank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.startPrank(admin);
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.executeCliffAndFlow(superToken, alice, bob);
    }

    function testCannotExecuteEndWithoutStreamRunning() public {
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        _createVestingScheduleWithDefaultData(alice, bob);
        vm.prank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.startPrank(admin);
        uint256 initialTimestamp = block.timestamp + 10 days + 1800;
        vm.warp(initialTimestamp);

        uint256 flowDelayCompensation = (block.timestamp - CLIFF_DATE) * uint96(FLOW_RATE);

        vm.expectEmit(true, true, true, true);
        emit Transfer(alice, bob, CLIFF_TRANSFER_AMOUNT + flowDelayCompensation);

        vm.expectEmit(true, true, true, true);
        emit VestingCliffAndFlowExecuted(
            superToken, alice, bob, CLIFF_DATE, FLOW_RATE, CLIFF_TRANSFER_AMOUNT, flowDelayCompensation
        );

        bool success = vestingScheduler.executeCliffAndFlow(superToken, alice, bob);
        assertTrue(success, "executeVesting should return true");
        vm.stopPrank();
        vm.startPrank(alice);
        superToken.deleteFlow(alice, bob);
        vm.stopPrank();
        vm.startPrank(admin);
        uint256 finalTimestamp = block.timestamp + 10 days - 3600;
        vm.warp(finalTimestamp);
        vm.expectEmit(true, true, true, true);
        emit VestingEndFailed(
            superToken, alice, bob, END_DATE
        );
        success = vestingScheduler.executeEndVesting(superToken, alice, bob);
        assertTrue(success, "executeCloseVesting should return true");
    }

    // # Vesting Scheduler 1.2 tests

    function testCreateAndExecuteImmediately() public {
        uint256 aliceInitialBalance = superToken.balanceOf(alice);
        uint256 bobInitialBalance = superToken.balanceOf(bob);

        // # Create schedule
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);

        vm.startPrank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);

        uint32 startAndCliffDate = uint32(block.timestamp);

        vm.expectEmit();
        emit VestingScheduleCreated(superToken, alice, bob, startAndCliffDate, startAndCliffDate, FLOW_RATE, END_DATE, CLIFF_TRANSFER_AMOUNT, 0);

        vestingScheduler.createVestingSchedule(
            superToken,
            bob,
            startAndCliffDate,
            startAndCliffDate,
            FLOW_RATE,
            CLIFF_TRANSFER_AMOUNT,
            END_DATE,
            EMPTY_CTX
        );
        // ---

        // # Execute start
        vm.expectEmit();
        emit Transfer(alice, bob, CLIFF_TRANSFER_AMOUNT);

        vm.expectEmit();
        emit VestingCliffAndFlowExecuted(
            superToken, alice, bob, startAndCliffDate, FLOW_RATE, CLIFF_TRANSFER_AMOUNT, uint256(0)
        );
        vm.stopPrank();

        vm.startPrank(admin);
        bool success = vestingScheduler.executeCliffAndFlow(superToken, alice, bob);
        vm.stopPrank();

        assertTrue(success, "executeVesting should return true");
        // ---

        // # Execute end
        uint256 finalTimestamp = END_DATE - 3600;
        vm.warp(finalTimestamp);

        uint256 timeDiffToEndDate = END_DATE > block.timestamp ? END_DATE - block.timestamp : 0;
        uint256 adjustedAmountClosing = timeDiffToEndDate * uint96(FLOW_RATE);

        vm.expectEmit();
        emit Transfer(alice, bob, adjustedAmountClosing);

        vm.expectEmit();
        emit VestingEndExecuted(
            superToken, alice, bob, END_DATE, adjustedAmountClosing, false
        );
        vm.startPrank(admin);
        success = vestingScheduler.executeEndVesting(superToken, alice, bob);
        vm.stopPrank();
        assertTrue(success, "executeCloseVesting should return true");

        uint256 aliceFinalBalance = superToken.balanceOf(alice);
        uint256 bobFinalBalance = superToken.balanceOf(bob);
        uint256 aliceShouldStream = (END_DATE - startAndCliffDate) * uint96(FLOW_RATE) + CLIFF_TRANSFER_AMOUNT;
        assertEq(aliceInitialBalance - aliceFinalBalance, aliceShouldStream, "(sender) wrong final balance");
        assertEq(bobFinalBalance, bobInitialBalance + aliceShouldStream, "(receiver) wrong final balance");
        // ---
    }

    function test_createScheduleFromAmountAndDuration_reverts() public {
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);

        vm.expectRevert();
        vestingScheduler.createVestingScheduleFromAmountAndDuration(
            superToken,
            bob,
            0,
            1209600,
            604800,
            uint32(block.timestamp),
            EMPTY_CTX
        );

        console.log("Revert with cliff and start in history.");
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingScheduleFromAmountAndDuration(
            superToken,
            bob,
            1 ether,
            1209600,
            0,
            uint32(block.timestamp - 1),
            EMPTY_CTX
        );

        console.log("Revert with overflow.");
        vm.expectRevert(); // todo: the right error
        vestingScheduler.createVestingScheduleFromAmountAndDuration(
            superToken,
            bob,
            type(uint256).max,
            1209600,
            0,
            uint32(block.timestamp),
            EMPTY_CTX
        );

        console.log("Revert with underflow/overflow.");
        vm.expectRevert(); // todo: the right error
        vestingScheduler.createVestingScheduleFromAmountAndDuration(
            superToken,
            bob,
            1 ether,
            type(uint32).max,
            0,
            uint32(block.timestamp),
            EMPTY_CTX
        );

        console.log("Revert with start date in history.");
        vm.expectRevert(IVestingSchedulerV2.TimeWindowInvalid.selector);
        vestingScheduler.createVestingScheduleFromAmountAndDuration(
            superToken,
            bob,
            1 ether,
            1209600,
            604800,
            uint32(block.timestamp - 1),
            EMPTY_CTX
        );
    }

    function testNewFunctionScheduleCreationWithoutCliff(uint8 randomizer) public {
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);

        vm.startPrank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.stopPrank();

        uint32 startDate = uint32(block.timestamp);
        uint256 totalVestedAmount = 105_840_000; // a value perfectly divisible by a week
        uint32 vestingDuration = 1 weeks;
        int96 expectedFlowRate = 175; // totalVestedAmount / vestingDuration
        uint32 expectedEndDate = startDate + vestingDuration;

        vm.expectEmit();
        emit VestingScheduleCreated(superToken, alice, bob, startDate, 0, expectedFlowRate, expectedEndDate, 0, 0);

        vm.startPrank(alice);
        bool useCtx = randomizer % 2 == 0;
        if (useCtx) {
            vestingScheduler.createVestingScheduleFromAmountAndDuration(
                superToken,
                bob,
                totalVestedAmount,
                vestingDuration,
                0,
                startDate,
                EMPTY_CTX
            );
        } else {
            vestingScheduler.createVestingScheduleFromAmountAndDuration(
                superToken,
                bob,
                totalVestedAmount,
                vestingDuration,
                0,
                startDate
            );
        }
        vm.stopPrank();
    }

    function testNewFunctionScheduleCreationWithCliff(uint8 randomizer) public {
        _setACL_AUTHORIZE_FULL_CONTROL(alice, FLOW_RATE);

        vm.startPrank(alice);
        superToken.increaseAllowance(address(vestingScheduler), type(uint256).max);
        vm.stopPrank();

        uint32 startDate = uint32(block.timestamp);
        uint256 totalVestedAmount = 103_680_000; // a value perfectly divisible
        uint32 vestingDuration = 1 weeks + 1 days;
        uint32 cliffPeriod = 1 days;

        int96 expectedFlowRate = 150; // (totalVestedAmount - cliffAmount) / (vestingDuration - cliffPeriod)
        uint256 expectedCliffAmount = 12960000;
        uint32 expectedCliffDate = startDate + cliffPeriod;
        uint32 expectedEndDate = startDate + vestingDuration;

        vm.expectEmit();
        emit VestingScheduleCreated(superToken, alice, bob, startDate, expectedCliffDate, expectedFlowRate, expectedEndDate, expectedCliffAmount, 0);

        vm.startPrank(alice);
        bool useCtx = randomizer % 2 == 0;
        if (useCtx) {
            vestingScheduler.createVestingScheduleFromAmountAndDuration(
                superToken,
                bob,
                totalVestedAmount,
                vestingDuration,
                cliffPeriod,
                startDate,
                EMPTY_CTX
            );
        } else {
            vestingScheduler.createVestingScheduleFromAmountAndDuration(
                superToken,
                bob,
                totalVestedAmount,
                vestingDuration,
                cliffPeriod,
                startDate
            );
        }
        vm.stopPrank();
    }

    function test_createScheduleFromAmountAndDuration_executeCliffAndFlow_executeEndVesting(
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 cliffPeriod,
        uint32 startDate,
        uint8 randomizer
    ) public {
        // Assume
        vm.assume(randomizer != 0);
        
        vm.assume(startDate == 0 || startDate >= block.timestamp);
        vm.assume(startDate < 2524600800 /* year 2050 */);

        vm.assume(totalDuration > vestingScheduler.MIN_VESTING_DURATION());
        vm.assume(cliffPeriod <= totalDuration - vestingScheduler.MIN_VESTING_DURATION());
        vm.assume(totalDuration < 18250 days /* 50 years */);

        uint256 beforeSenderBalance = superToken.balanceOf(alice);
        uint256 beforeReceiverBalance = superToken.balanceOf(bob);

        vm.assume(totalAmount > 1);
        vm.assume(totalAmount >= totalDuration);
        vm.assume(totalAmount / totalDuration <= SafeCast.toUint256(type(int96).max));
        vm.assume(totalAmount <= beforeSenderBalance);

        IVestingSchedulerV2.VestingSchedule memory nullSchedule = vestingScheduler.getVestingSchedule(address(superToken), alice, bob);
        assertTrue(nullSchedule.endDate == 0, "Schedule should not exist");

        // Arrange
        IVestingSchedulerV2.VestingSchedule memory expectedSchedule = _getExpectedScheduleFromAmountAndDuration(
            totalAmount,
            totalDuration,
            cliffPeriod,
            startDate
        );
        uint32 expectedCliffDate = cliffPeriod == 0 ? 0 : expectedSchedule.cliffAndFlowDate;
        uint32 expectedStartDate = startDate == 0 ? uint32(block.timestamp) : startDate;

        // Assume we're not getting liquidated at the end:
        vm.assume(totalAmount <= (beforeSenderBalance - vestingScheduler.END_DATE_VALID_BEFORE() * SafeCast.toUint256(expectedSchedule.flowRate)));

        console.log("Total amount: %s", totalAmount);
        console.log("Total duration: %s", totalDuration);
        console.log("Cliff period: %s", cliffPeriod);
        console.log("Start date: %s", startDate);
        console.log("Randomizer: %s", randomizer);
        console.log("Expected start date: %s", expectedStartDate);
        console.log("Expected cliff date: %s", expectedCliffDate);
        console.log("Expected cliff & flow date: %s", expectedSchedule.cliffAndFlowDate);
        console.log("Expected end date: %s", expectedSchedule.endDate);
        console.log("Expected flow rate: %s", SafeCast.toUint256(expectedSchedule.flowRate));
        console.log("Expected cliff amount: %s", expectedSchedule.cliffAmount);
        console.log("Expected remainder amount: %s", expectedSchedule.remainderAmount);
        console.log("Sender balance: %s", beforeSenderBalance);

        _arrangeAllowances(alice, expectedSchedule.flowRate);

        vm.expectEmit();
        emit VestingScheduleCreated(superToken, alice, bob, expectedStartDate, expectedCliffDate, expectedSchedule.flowRate, expectedSchedule.endDate, expectedSchedule.cliffAmount, expectedSchedule.remainderAmount);

        // Act
        vm.startPrank(alice);
        if (startDate == 0 && randomizer % 2 == 0) {
            console.log("Using the overload without start date.");
            vestingScheduler.createVestingScheduleFromAmountAndDuration(
                superToken,
                bob,
                totalAmount,
                totalDuration,
                cliffPeriod
            );
        } else {
            if (randomizer % 3 == 0) {
                console.log("Using the overload without superfluid context.");
                vestingScheduler.createVestingScheduleFromAmountAndDuration(
                    superToken,
                    bob,
                    totalAmount,
                    totalDuration,
                    cliffPeriod,
                    startDate
                );
            } else {
                console.log("Using the overload with superfluid context.");
                vestingScheduler.createVestingScheduleFromAmountAndDuration(
                    superToken,
                    bob,
                    totalAmount,
                    totalDuration,
                    cliffPeriod,
                    startDate,
                    EMPTY_CTX
                );
            }
        }
        vm.stopPrank();

        // Assert
        IVestingSchedulerV2.VestingSchedule memory actualSchedule = vestingScheduler.getVestingSchedule(address(superToken), alice, bob);
        assertEq(actualSchedule.cliffAndFlowDate, expectedSchedule.cliffAndFlowDate, "schedule created: cliffAndFlowDate not expected");
        assertEq(actualSchedule.flowRate, expectedSchedule.flowRate, "schedule created: flowRate not expected");
        assertEq(actualSchedule.cliffAmount, expectedSchedule.cliffAmount, "schedule created: cliffAmount not expected");
        assertEq(actualSchedule.endDate, expectedSchedule.endDate, "schedule created: endDate not expected");
        assertEq(actualSchedule.remainderAmount, expectedSchedule.remainderAmount, "schedule created: remainderAmount not expected");

        // Act
        vm.warp(expectedSchedule.cliffAndFlowDate + (vestingScheduler.START_DATE_VALID_AFTER() - (vestingScheduler.START_DATE_VALID_AFTER() / randomizer)));
        assertTrue(vestingScheduler.executeCliffAndFlow(superToken, alice, bob));

        // Assert
        actualSchedule = vestingScheduler.getVestingSchedule(address(superToken), alice, bob);
        assertEq(actualSchedule.cliffAndFlowDate, 0, "schedule started: cliffAndFlowDate not expected");
        assertEq(actualSchedule.cliffAmount, 0, "schedule started: cliffAmount not expected");
        assertEq(actualSchedule.flowRate, expectedSchedule.flowRate, "schedule started: flowRate not expected");
        assertEq(actualSchedule.endDate, expectedSchedule.endDate, "schedule started: endDate not expected");
        assertEq(actualSchedule.remainderAmount, expectedSchedule.remainderAmount, "schedule started: remainderAmount not expected");

        // Act
        vm.warp(expectedSchedule.endDate - (vestingScheduler.END_DATE_VALID_BEFORE() - (vestingScheduler.END_DATE_VALID_BEFORE() / randomizer)));
        assertTrue(vestingScheduler.executeEndVesting(superToken, alice, bob));

        actualSchedule = vestingScheduler.getVestingSchedule(address(superToken), alice, bob);
        assertEq(actualSchedule.cliffAndFlowDate, 0, "schedule ended: cliffAndFlowDate not expected");
        assertEq(actualSchedule.cliffAmount, 0, "schedule ended: cliffAmount not expected");
        assertEq(actualSchedule.flowRate, 0, "schedule ended: flowRate not expected");
        assertEq(actualSchedule.endDate, 0, "schedule ended: endDate not expected");
        assertEq(actualSchedule.remainderAmount, 0, "schedule ended: remainderAmount not expected");

        // Assert
        uint256 afterSenderBalance = superToken.balanceOf(alice);
        uint256 afterReceiverBalance = superToken.balanceOf(bob);

        assertEq(afterSenderBalance, beforeSenderBalance - totalAmount, "Sender balance should decrease by totalAmount");
        assertEq(afterReceiverBalance, beforeReceiverBalance + totalAmount, "Receiver balance should increase by totalAmount");

        vm.warp(type(uint32).max);
        assertEq(afterSenderBalance, superToken.balanceOf(alice), "After the schedule has ended, the sender's balance should never change.");
    }

    function _getExpectedSchedule(
        uint32 startDate,
        uint32 cliffDate,
        int96 flowRate,
        uint256 cliffAmount,
        uint32 endDate
    ) public view returns (IVestingSchedulerV2.VestingSchedule memory expectedSchedule) {
        if (startDate == 0) {
            startDate = uint32(block.timestamp);
        }

        uint32 cliffAndFlowDate = cliffDate == 0 ? startDate : cliffDate;

        expectedSchedule = IVestingSchedulerV2.VestingSchedule({
            cliffAndFlowDate: cliffAndFlowDate,
            flowRate: flowRate,
            cliffAmount: cliffAmount,
            endDate: endDate,
            remainderAmount: 0
        });
    }

    function _getExpectedScheduleFromAmountAndDuration(
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 cliffPeriod,
        uint32 startDate
    ) public view returns (IVestingSchedulerV2.VestingSchedule memory expectedSchedule) {
        if (startDate == 0) {
            startDate = uint32(block.timestamp);
        }

        int96 flowRate = SafeCast.toInt96(SafeCast.toInt256(totalAmount / totalDuration));

        uint32 cliffDate;
        uint32 cliffAndFlowDate;
        uint256 cliffAmount;
        if (cliffPeriod > 0) {
            cliffDate = startDate + cliffPeriod;
            cliffAmount = cliffPeriod * SafeCast.toUint256(flowRate);
            cliffAndFlowDate = cliffDate;
        } else {
            cliffDate = 0;
            cliffAmount = 0;
            cliffAndFlowDate = startDate;
        }

        uint32 endDate = startDate + totalDuration;

        uint256 remainderAmount = totalAmount - SafeCast.toUint256(flowRate) * totalDuration;

        expectedSchedule = IVestingSchedulerV2.VestingSchedule({
            cliffAndFlowDate: cliffAndFlowDate,
            flowRate: flowRate,
            cliffAmount: cliffAmount,
            endDate: endDate,
            remainderAmount: remainderAmount
        });
    }
}
