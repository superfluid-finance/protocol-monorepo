{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.InstantDistributionAgreement_test (tests) where

import qualified Money.Systems.Superfluid.Instances.Simple.System as SF
--
import           Money.Systems.Superfluid.TokenTester

-- =====================================================================================================================
-- * Testing Utilities
--

tstep = 1000 :: SF.SimpleTimestamp

-- 1x test unit for transfering amount
u1x = SF.toWad(5 :: Double)

-- =====================================================================================================================
-- * Test Scenarios
--

simple1to2ScenarioTest = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 1to2 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol] = testAddresses ctx
    expectZeroTotalValue

    -- T0: setup subscription from bob to alice, alice distribute 1x
    runToken $ SF.updateProportionalDistributionSubscription bob alice 0 1000
    runToken $ SF.distributeProportionally alice 0 u1x
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqual'        constInitBalance
    expectZeroTotalValue

    -- T2: setup subscription from carol to alice, alice distribute 3x
    runToken $ SF.updateProportionalDistributionSubscription carol alice 0 2000
    runToken $ SF.distributeProportionally alice 0 (3*u1x)
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqual' (constInitBalance - 4 * u1x)
    expectAccountBalanceTo bob   $ assertEqual' (constInitBalance + 2 * u1x)
    expectAccountBalanceTo carol $ assertEqual' (constInitBalance + 2 * u1x)
    expectZeroTotalValue
      )

tests = createTokenTestSuite "InstantDistributionAgreement System Testsuite"
    [ simple1to2ScenarioTest
    ]
