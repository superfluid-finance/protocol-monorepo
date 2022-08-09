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

    -- T1: alice distribute 5x
    runToken $ SF.distributeFlow alice 0 (5*fr1x)
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 5 * u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + 1 * u1x)
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance + 4 * u1x)

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
    expectAccountBalanceTo alice $ assertEqualWith constInitBalance
    expectAccountBalanceTo bob   $ assertEqualWith constInitBalance
    expectAccountBalanceTo carol $ assertEqualWith constInitBalance

    -- T1: alice distribute 1x flow
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqualWith        constInitBalance

    -- T1a: setup subscription from carol to alice
    runToken $ SF.updateProportionalDistributionSubscription carol alice 0 4000
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqualWith        constInitBalance

    -- T1b: alice distribute 5x flow
    runToken $ SF.distributeFlow alice 0 (5*fr1x)
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqualWith        constInitBalance

    -- T2: time travel
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 6 * u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + 2 * u1x)
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance + 4 * u1x)

    -- T3: alice distribute 10x flow, and time travel
    runToken $ SF.distributeFlow alice 0 (10*fr1x)
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 16 * u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance +  4 * u1x)
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance + 12 * u1x)

    expectZeroTotalValue)

multi1to2and1to1ScenarioTest = TokenTestCase TokenTestSpec
    { testCaseLabel = "Multi 1to1 1to2 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol", "dan"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol, dan] = testAddresses ctx
    expectZeroTotalValue

    -- T0: setup subscription from bob to alice, and carol to alice
    runToken $ SF.updateProportionalDistributionSubscription bob alice 0 1000
    runToken $ SF.updateProportionalDistributionSubscription carol alice 0 4000

    -- T1: alice distribute 5x
    runToken $ SF.distributeFlow alice 0 (5*fr1x)
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 5 * u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + 1 * u1x)
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance + 4 * u1x)
    expectAccountBalanceTo dan   $ assertEqualWith constInitBalance

    -- T2: setup subscription from dan to alice
    runToken $ SF.updateProportionalDistributionSubscription dan alice 1 1000
    runToken $ SF.distributeFlow alice 1 (1*fr1x)
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 11 * u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance +  2 * u1x)
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance +  8 * u1x)
    expectAccountBalanceTo dan   $ assertEqualWith (constInitBalance +  1 * u1x)

    expectZeroTotalValue
      )

tests = createTokenTestSuite "ConstantFlowDistributionAgreement System Testsuite"
    [ simple1to2ScenarioTest1
    , simple1to2ScenarioTest2
    , multi1to2and1to1ScenarioTest
    ]
