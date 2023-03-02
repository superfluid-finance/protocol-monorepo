// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import {
    FoundrySuperfluidTester,
    SuperToken
} from "../FoundrySuperfluidTester.sol";
import {
    ISuperfluidToken
} from "../../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";

/// @title ConstantFlowAgreementV1LiquidationsTest
/// @author Superfluid
/// @notice A contract for testing that liquidations work as expected
contract ConstantFlowAgreementV1LiquidationsTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;

    event AgreementLiquidatedV2(
        address indexed agreementClass,
        bytes32 id,
        address indexed liquidatorAccount,
        address indexed targetAccount,
        address rewardAmountReceiver,
        uint256 rewardAmount,
        int256 targetAccountBalanceDelta,
        bytes liquidationTypeData
    );

    event Transfer(
        address indexed from,
        address indexed to,
        uint256 indexed byba
    );

    event FlowUpdated(
        ISuperfluidToken indexed token,
        address indexed sender,
        address indexed receiver,
        int96 flowRate,
        int256 totalSenderFlowRate,
        int256 totalReceiverFlowRate,
        bytes userData
    );

    event FlowUpdatedExtension(address indexed flowOperator, uint256 deposit);

    constructor() FoundrySuperfluidTester(5) {}

    function helper_Get_Expected_Reward_Amount_For_Solvent_Liquidation(
        int96 netFlowRate,
        uint256 timePassed,
        int256 senderAvailableBalanceBefore,
        uint256 senderAccountDeposit,
        uint256 flowDeposit
    ) internal returns (int256) {
        int256 adjustedRewardAmount = int256(netFlowRate) * int256(timePassed);
        int256 intSenderAccountDeposit = int256(senderAccountDeposit);
        int256 intFlowDeposit = int256(flowDeposit);
        int256 totalRewardLeft = senderAvailableBalanceBefore +
            int256(senderAccountDeposit) +
            adjustedRewardAmount;
        return (intFlowDeposit * totalRewardLeft) / intSenderAccountDeposit;
    }

    function helper_Get_Expected_Reward_Amount_For_Insolvent_Liquidation(
        int96 netFlowRate,
        uint256 timePassed,
        int256 senderAvailableBalanceBefore,
        uint256 senderAccountDeposit,
        uint256 flowDeposit
    ) internal returns (int256 rewardAmount, int256 bailoutAmount) {
        int256 adjustedRewardAmount = int256(netFlowRate) * int256(timePassed);
        rewardAmount = int256(flowDeposit);
        bailoutAmount =
            senderAvailableBalanceBefore +
            int256(senderAccountDeposit) *
            -1 -
            adjustedRewardAmount;
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/

    // @note still need to figure out how to test asserting transfer events here too
    function assert_Event_Transfer(
        address _emittingAddress,
        address _expectedFrom,
        address _expectedTo,
        uint256 _expectedValue
    ) public {
        vm.expectEmit(true, true, false, true, _emittingAddress);

        emit Transfer(_expectedFrom, _expectedTo, _expectedValue);
    }

    function assert_Event_FlowUpdated(
        ISuperfluidToken _superToken,
        address emittingAddress,
        address expectedSender,
        address expectedReceiver,
        int96 expectedFlowRate,
        int256 expectedTotalSenderFlowRate,
        int256 expectedTotalReceiverFlowRate,
        bytes memory expectedUserData
    ) public {
        vm.expectEmit(true, true, true, true, emittingAddress);

        emit FlowUpdated(
            _superToken,
            expectedSender,
            expectedReceiver,
            expectedFlowRate,
            expectedTotalSenderFlowRate,
            expectedTotalReceiverFlowRate,
            expectedUserData
        );
    }

    function assert_Event_AgreementLiquidatedV2(
        address emittingAddress,
        address expectedAgreementClass,
        bytes32 expectedId,
        address expectedLiquidatorAccount,
        address expectedTargetAccount,
        address expectedRewardAmountReceiver,
        uint256 expectedRewardAmount,
        int256 expectedTargetAccountBalanceDelta,
        bytes memory expectedLiquidationTypeData
    ) public {
        vm.expectEmit(true, true, true, false, emittingAddress);

        emit AgreementLiquidatedV2(
            expectedAgreementClass,
            expectedId,
            expectedLiquidatorAccount,
            expectedTargetAccount,
            expectedRewardAmountReceiver,
            expectedRewardAmount,
            expectedTargetAccountBalanceDelta,
            expectedLiquidationTypeData
        );
    }

    function assert_Event_FlowUpdatedExtension(
        address emittingAddress,
        address expectedFlowOperator,
        uint256 expectedDeposit
    ) public {
        vm.expectEmit(true, true, true, false, emittingAddress);

        emit FlowUpdatedExtension(expectedFlowOperator, expectedDeposit);
    }

    function test_Passing_PIC_Liquidation(uint32 a) public {
        int96 flowRate = assume_Valid_Flow_Rate(a);
        assert_Event_FlowUpdated(
            superToken,
            address(sf.cfa),
            alice,
            bob,
            flowRate,
            -flowRate,
            flowRate,
            ""
        );
        uint256 nftId = uint256(keccak256(abi.encode(alice, bob)));

        int96 senderNetFlowRateBefore = superToken.getNetFlowRate(alice);
        int96 receiverNetFlowRateBefore = superToken.getNetFlowRate(bob);

        vm.startPrank(alice);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();

        assert_Modify_Flow_And_Net_Flow_Is_Expected(
            alice,
            bob,
            flowRate,
            senderNetFlowRateBefore,
            receiverNetFlowRateBefore
        );

        assert_Modify_Flow_And_Flow_Info_Is_Expected(
            alice,
            bob,
            flowRate,
            block.timestamp,
            0
        );

        assert_Global_Invariants();
        (
            int256 senderBalance,
            uint256 deposit,
            ,
            uint256 lastUpdatedAt
        ) = superToken.realtimeBalanceOfNow(alice);

        // time to drain balance to 0
        uint256 timeToZeroBalance = uint256(senderBalance) /
            uint256(uint96(flowRate));
        // go to when balance is 0
        vm.warp(block.timestamp + timeToZeroBalance + 1);

        // get expected reward amount
        int96 senderNetFlowRate = superToken.getNetFlowRate(alice);
        (, , uint256 flowDeposit, ) = superToken.getFlowInfo(alice, bob);
        int256 expectedRewardAmount = helper_Get_Expected_Reward_Amount_For_Solvent_Liquidation(
                senderNetFlowRate,
                block.timestamp - lastUpdatedAt,
                senderBalance,
                deposit,
                flowDeposit
            );

        // liquidate alice->bob flow as admin (PIC receives reward)
        (int256 defaultRewardAddressBalanceBeforeLiquidation, , , ) = superToken
            .realtimeBalanceOfNow(defaultRewardAddress);

        assert_Event_AgreementLiquidatedV2(
            address(superToken),
            address(sf.cfa),
            keccak256(abi.encodePacked(alice, bob)),
            admin,
            alice,
            defaultRewardAddress,
            uint256(expectedRewardAmount),
            -expectedRewardAmount,
            abi.encode(1, 0)
        );

        assert_Event_Transfer(
            address(superToken.constantInflowNFT()),
            bob,
            address(0),
            nftId
        );
        assert_Event_Transfer(
            address(superToken.constantOutflowNFT()),
            alice,
            address(0),
            nftId
        );

        vm.startPrank(admin);
        superToken.deleteFlow(alice, bob);
        vm.stopPrank();
        (int256 defaultRewardAddressBalanceAfterLiquidation, , , ) = superToken
            .realtimeBalanceOfNow(defaultRewardAddress);

        assertEq(
            defaultRewardAddressBalanceBeforeLiquidation + expectedRewardAmount,
            defaultRewardAddressBalanceAfterLiquidation
        );

        assert_Global_Invariants();
    }

    function test_Passing_Pleb_Liquidation(uint32 a) public {
        int96 absFlowRate = helper_Create_Flow_And_Assert_Global_Invariants(
            alice,
            bob,
            a
        );
        (
            int256 senderBalance,
            uint256 deposit,
            ,
            uint256 lastUpdatedAt
        ) = superToken.realtimeBalanceOfNow(alice);
        uint256 timeToZeroBalance = uint256(senderBalance) /
            uint256(uint96(absFlowRate));

        (, , uint256 flowDeposit, ) = superToken.getFlowInfo(alice, bob);

        (uint256 liqPeriod, uint256 patPeriod) = sf.governance.getPPPConfig(
            sf.host,
            superToken
        );

        int96 senderNetFlowRate = superToken.getNetFlowRate(alice);

        // this is what the protocol views the total outflow rate as
        int256 totalCFAOutflowRate = int256(deposit) / int256(liqPeriod);
        int256 amountToGoNegative = totalCFAOutflowRate * int256(patPeriod);
        uint256 amountOfTimeToPass = uint256(amountToGoNegative) /
            uint256(uint96(absFlowRate));

        vm.warp(block.timestamp + timeToZeroBalance + amountOfTimeToPass + 1);

        int256 expectedRewardAmount = helper_Get_Expected_Reward_Amount_For_Solvent_Liquidation(
                senderNetFlowRate,
                block.timestamp - lastUpdatedAt,
                senderBalance,
                deposit,
                flowDeposit
            );

        // liquidate alice->bob flow as admin (PIC receives reward)
        (int256 adminBalanceBefore, , , ) = superToken.realtimeBalanceOfNow(
            admin
        );

        assert_Event_AgreementLiquidatedV2(
            address(superToken),
            address(sf.cfa),
            keccak256(abi.encodePacked(alice, bob)),
            admin,
            alice,
            admin,
            uint256(expectedRewardAmount),
            -expectedRewardAmount,
            abi.encode(1, 1)
        );

        vm.startPrank(admin);
        superToken.deleteFlow(alice, bob);
        vm.stopPrank();
        (int256 adminBalanceAfter, , , ) = superToken.realtimeBalanceOfNow(
            admin
        );

        assertEq(adminBalanceBefore + expectedRewardAmount, adminBalanceAfter);

        assert_Global_Invariants();
    }

    function test_Passing_Pirate_Liquidation(uint32 a) public {
        int96 absFlowRate = helper_Create_Flow_And_Assert_Global_Invariants(
            alice,
            bob,
            a
        );

        (
            int256 senderBalance,
            uint256 deposit,
            ,
            uint256 lastUpdatedAt
        ) = superToken.realtimeBalanceOfNow(alice);
        uint256 timeToZeroBalance = uint256(senderBalance) /
            uint256(uint96(absFlowRate));

        (, , uint256 flowDeposit, ) = superToken.getFlowInfo(alice, bob);

        (uint256 liqPeriod, uint256 patPeriod) = sf.governance.getPPPConfig(
            sf.host,
            superToken
        );

        int96 senderNetFlowRate = superToken.getNetFlowRate(alice);

        // this is what the protocol views the total outflow rate as
        int256 totalCFAOutflowRate = int256(deposit) / int256(liqPeriod);
        //
        int256 amountToGoNegative = totalCFAOutflowRate * int256(liqPeriod) * 2;
        uint256 amountOfTimeToPass = uint256(amountToGoNegative) /
            uint256(uint96(absFlowRate));

        vm.warp(block.timestamp + timeToZeroBalance + amountOfTimeToPass);
        (senderBalance, deposit, , ) = superToken.realtimeBalanceOfNow(alice);
        (
            int256 expectedRewardAmount,
            int256 expectedBailoutAmount
        ) = helper_Get_Expected_Reward_Amount_For_Insolvent_Liquidation(
                senderNetFlowRate,
                block.timestamp - lastUpdatedAt,
                senderBalance,
                deposit,
                flowDeposit
            );

        // liquidate alice->bob flow as admin (PIC receives reward)
        (int256 adminBalanceBefore, , , ) = superToken.realtimeBalanceOfNow(
            admin
        );

        assert_Event_AgreementLiquidatedV2(
            address(superToken),
            address(sf.cfa),
            keccak256(abi.encodePacked(alice, bob)),
            admin,
            alice,
            admin,
            uint256(expectedRewardAmount),
            expectedBailoutAmount,
            abi.encode(1, 2)
        );

        vm.startPrank(admin);
        superToken.deleteFlow(alice, bob);
        vm.stopPrank();
        (int256 adminBalanceAfter, , , ) = superToken.realtimeBalanceOfNow(
            admin
        );

        assertEq(adminBalanceAfter, adminBalanceBefore + expectedRewardAmount);

        assert_Global_Invariants();
    }
}
