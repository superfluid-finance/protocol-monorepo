{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.InstantDistributionAgreement_test (tests) where

import           Control.Monad.IO.Class
import           Data.Default
import           Data.Maybe                                                           (fromMaybe)
import           Lens.Micro
import           Test.Hspec                                                           (HasCallStack)
import           Test.HUnit

import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantTransfer as ITMUD
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex    as PDIDX
--
import qualified Money.Systems.Superfluid.Instances.Simple.System                     as SF
--
import           Money.Systems.Superfluid.TokenTester

-- =====================================================================================================================
-- * Testing Utilities
--

-- 1x test unit for transfering amount
u1x = SF.toWad(42 :: Double)

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

    -- T0: setup subscription from bob alice
    runToken $ SF.updateProportionalDistributionSubscription bob alice 0 100

    -- T1: alice distribute 1x
    runToken $ SF.distributeProportionally alice 0 u1x
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x) "alice should send 1x money"
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x) "bob should receive 1x money"
    expectAccountBalanceTo carol $ assertEqualWith        constInitBalance  "carol should not receive money yet"

      )

tests = createTokenTestSuite "InstantDistributionAgreement System Testsuite"
    [ simple1to2ScenarioTest
    ]
