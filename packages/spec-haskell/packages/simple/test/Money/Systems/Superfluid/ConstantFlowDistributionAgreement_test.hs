{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.ConstantFlowDistributionAgreement_test (tests) where

-- import           Control.Monad.IO.Class
-- import           Data.Default
-- import           Data.Maybe                                                           (fromMaybe)
-- import           Lens.Micro
-- import           Test.Hspec                                                           (HasCallStack)
-- import           Test.HUnit

-- import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantValue as IVMUD
-- import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex    as PDIDX
--
import qualified Money.Systems.Superfluid.Instances.Simple.System as SF
--
import           Money.Systems.Superfluid.TokenTester

-- =====================================================================================================================
-- * Testing Utilities
--

tstep = 1000 :: SF.SimpleTimestamp

-- 1x test unit for flow rate
fr1x = SF.toWad(0.001 :: Double)

u1x = fromIntegral tstep * fr1x

-- =====================================================================================================================
-- * Test Scenarios
--

simple1to2ScenarioTest1 = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 1to2 Scenario Test 1"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol] = testAddresses ctx
    expectZeroTotalValue

    -- T0: setup subscription from bob to alice, and carol to alice
    runToken $ SF.updateProportionalDistributionSubscription bob alice 0 1000
    runToken $ SF.updateProportionalDistributionSubscription carol alice 0 4000

    -- T1: alice distribute 1x
    runToken $ SF.distributeFlow alice 0 (5*fr1x)
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 5 * u1x) "alice should've send 5x money"
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + 1 * u1x) "bob should've receive 2x money"
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance + 4 * u1x) "carol should've received 2x money yet"

    expectZeroTotalValue
      )

simple1to2ScenarioTest2 = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 1to2 Scenario Test 2"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol] = testAddresses ctx
    expectZeroTotalValue

    -- T0: setup subscription from bob to alice, alice distribute 1x flow
    runToken $ SF.updateProportionalDistributionSubscription bob alice 0 1000
    runToken $ SF.distributeFlow alice 0 fr1x
    expectAccountBalanceTo alice $ assertEqualWith constInitBalance "T0 alice should not send money yet"
    expectAccountBalanceTo bob   $ assertEqualWith constInitBalance "T0 bob should not receive money yet"
    expectAccountBalanceTo carol $ assertEqualWith constInitBalance "T0 carol should not receive money yet"

    -- T1: alice distribute 1x flow
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x) "T1 alice should've sent 1x money"
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x) "T1 bob should've received 1x money"
    expectAccountBalanceTo carol $ assertEqualWith        constInitBalance  "T1 carol should've received money"

    -- T1a: setup subscription from carol to alice, alice distribute 5x flow
    runToken $ SF.updateProportionalDistributionSubscription carol alice 0 4000
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x) "T1a alice should not send more money yet"
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x) "T1a bob should not receive more money yet"
    expectAccountBalanceTo carol $ assertEqualWith        constInitBalance  "T1a carol should not receive more money yet"

    -- T1b: setup subscription from carol to alice, alice distribute 5x flow
    runToken $ SF.distributeFlow alice 0 (5*fr1x)
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x) "T1b alice should not send more money yet"
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x) "T1b bob should not receive more money yet"
    expectAccountBalanceTo carol $ assertEqualWith        constInitBalance  "T1b carol should not receive more money yet"

    -- T2: alice distribute 1x
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 6 * u1x) "T2 alice should've send 5x money"
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + 2 * u1x) "T2 bob should've receive 1x money"
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance + 4 * u1x) "T2 carol should've received 4x money"

    expectZeroTotalValue
      )

tests = createTokenTestSuite "ConstantFlowDistributionAgreement System Testsuite"
    [ simple1to2ScenarioTest1
    , simple1to2ScenarioTest2
    ]
