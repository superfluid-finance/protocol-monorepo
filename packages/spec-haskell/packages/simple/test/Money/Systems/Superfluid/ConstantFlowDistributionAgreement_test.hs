{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.ConstantFlowDistributionAgreement_test (tests) where

-- import           Control.Monad.IO.Class
-- import           Data.Default
-- import           Data.Maybe                                                           (fromMaybe)
-- import           Lens.Micro
-- import           Test.Hspec                                                           (HasCallStack)
-- import           Test.HUnit

-- import qualified Money.Systems.Superfluid.MonetaryUnitData.InstantValue as IVMUD
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

scenario1Pub2SubsSimple = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 1 Pub 2 Subs Scenario"
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
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - 5 * u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance + 1 * u1x)
    expectAccountBalanceTo carol $ assertEqual' (constInitBalance + 4 * u1x)

    expectZeroTotalValue
      )

scenario1Pub2SubsComplex = TokenTestCase TokenTestSpec
    { testCaseLabel = "Complex 1 Pub 2 Subs Scenario"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol] = testAddresses ctx
    expectZeroTotalValue

    -- T0: setup subscription from bob to alice, alice distribute 1x flow
    runToken $ SF.updateProportionalDistributionSubscription bob alice 0 1000
    runToken $ SF.distributeFlow alice 0 fr1x
    expectAccountBalanceTo alice $ assertEqual' constInitBalance
    expectAccountBalanceTo bob   $ assertEqual' constInitBalance
    expectAccountBalanceTo carol $ assertEqual' constInitBalance

    -- T1: alice distribute 1x flow
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqual'        constInitBalance

    -- T1a: setup subscription from carol to alice
    runToken $ SF.updateProportionalDistributionSubscription carol alice 0 4000
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqual'        constInitBalance

    -- T1b: alice distribute 5x flow
    runToken $ SF.distributeFlow alice 0 (5*fr1x)
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqual'        constInitBalance

    -- T2: time travel
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - 6 * u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance + 2 * u1x)
    expectAccountBalanceTo carol $ assertEqual' (constInitBalance + 4 * u1x)

    -- T3: alice distribute 10x flow, and time travel
    runToken $ SF.distributeFlow alice 0 (10*fr1x)
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - 16 * u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance +  4 * u1x)
    expectAccountBalanceTo carol $ assertEqual' (constInitBalance + 12 * u1x)

    expectZeroTotalValue)

scenario2Pubs1SubsSimple = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 2 Pubs 1 Sub Scenario"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol] = testAddresses ctx
    expectZeroTotalValue

    -- T0: setup subscription from carol to alice, and carol to bob
    runToken $ SF.updateProportionalDistributionSubscription carol alice 0 1000
    runToken $ SF.updateProportionalDistributionSubscription carol bob 0 4000

    -- T1: alice distribute 5x
    runToken $ SF.distributeFlow alice 0 (5*fr1x)
    runToken $ SF.distributeFlow bob 0 (3*fr1x)
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - 5 * u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance - 3 * u1x)
    expectAccountBalanceTo carol $ assertEqual' (constInitBalance + 8 * u1x)

    expectZeroTotalValue
      )

multi1Pub2x1SubsScenarioTest = TokenTestCase TokenTestSpec
    { testCaseLabel = "Multi indexes 1 Pub 2x1 Subs Scenario"
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
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - 5 * u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance + 1 * u1x)
    expectAccountBalanceTo carol $ assertEqual' (constInitBalance + 4 * u1x)
    expectAccountBalanceTo dan   $ assertEqual' constInitBalance

    -- T2: setup subscription from dan to alice
    runToken $ SF.updateProportionalDistributionSubscription dan alice 1 1000
    runToken $ SF.distributeFlow alice 1 (1*fr1x)
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - 11 * u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance +  2 * u1x)
    expectAccountBalanceTo carol $ assertEqual' (constInitBalance +  8 * u1x)
    expectAccountBalanceTo dan   $ assertEqual' (constInitBalance +  1 * u1x)

    expectZeroTotalValue
      )

tests = createTokenTestSuite "ConstantFlowDistributionAgreement System Testsuite"
    [ scenario1Pub2SubsSimple
    , scenario1Pub2SubsComplex
    , scenario2Pubs1SubsSimple
    , multi1Pub2x1SubsScenarioTest
    ]
