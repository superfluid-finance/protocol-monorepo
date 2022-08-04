{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.InstantDistributionAgreement_test (tests) where

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
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x) "alice should've sent 1x money"
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x) "bob should've received 1x money"
    expectAccountBalanceTo carol $ assertEqualWith        constInitBalance  "carol should've received money yet"

    -- T2: setup subscription from carol to alice, alice distribute 3x
    runToken $ SF.updateProportionalDistributionSubscription carol alice 0 2000
    runToken $ SF.distributeProportionally alice 0 (3*u1x)
    timeTravel $ tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 4 * u1x) "alice should've send 4x money"
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + 2 * u1x) "bob should've receive 2x money"
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance + 2 * u1x) "carol should've received 2x money yet"

      )

tests = createTokenTestSuite "InstantDistributionAgreement System Testsuite"
    [ simple1to2ScenarioTest
    ]
