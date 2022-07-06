{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.System_unit (tests) where

import           Control.Monad.IO.Class
import           Test.Framework.Providers.HUnit                            (hUnitTestToTests)
import           Test.HUnit                                                (assertEqual)

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement as CFA
--
import qualified Money.Systems.Superfluid.Instances.Simple.System          as SF
--
import           Money.Systems.Superfluid.TokenTester


simple1to1ScenarioTest = TokenTestCase TokenTestSpec
    { testLabel = "Simple 1to1 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol] = testAddresses ctx
    -- T0: test initial condition
    expectZeroTotalLiquidity
    accounts' <- runToken SF.listAccounts
    liftIO $ assertEqual "expected number of accounts" 4 (length accounts')

    -- T1: test initial condition
    -- creating flow: alice -> bob @ 0.0001/s
    runToken $ SF.updateFlow (CFA.CFAPartiesF alice bob) (SF.toWad (0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have -1x net flowrate" alice (== SF.toWad(-0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" bob (== SF.toWad(0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have zero net flowrate" carol (== SF.toWad(0.0000 :: Double))
    expectCFAFlowRateTo "alice -> bob should have 1x flowrate" (alice, bob) (== SF.toWad(0.0001 :: Double))
    expectCFAFlowRateTo "alice -> carol should have 1x flowrate" (alice, carol) (== SF.toWad(0.0000 :: Double))
    expectCFAFlowRateTo "bob -> carol should have 1x flowrate" (bob, carol) (== SF.toWad(0.0000 :: Double))

    -- T2: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo "alice should send money" alice (< constInitBalance)
    expectAccountBalanceTo "bob should receive money" bob (> constInitBalance)
    expectAccountBalanceTo "carol should be the same" carol (== constInitBalance)
    expectZeroTotalLiquidity
    )

simple1to2ScenarioTest = TokenTestCase TokenTestSpec
    { testLabel = "Simple 1to2 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    -- T0: test initial condition
    let [alice, bob, carol] = testAddresses ctx
    runToken $ SF.updateFlow (CFA.CFAPartiesF alice bob) (SF.toWad (0.0001 :: Double))
    runToken $ SF.updateFlow (CFA.CFAPartiesF alice carol) (SF.toWad (0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have -2x net flowrate" alice (== SF.toWad(-0.0002 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" bob (== SF.toWad(0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" carol (== SF.toWad(0.0001 :: Double))
    expectCFAFlowRateTo "alice -> bob should have 1x flowrate" (alice, bob) (== SF.toWad(0.0001 :: Double))
    expectCFAFlowRateTo "alice -> carol should have 1x flowrate" (alice, carol) (== SF.toWad(0.0001 :: Double))
    expectCFAFlowRateTo "bob -> carol should have 1x flowrate" (bob, carol) (== SF.toWad(0.0000 :: Double))

    -- T1: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo "alice should send money" alice (< constInitBalance)
    expectAccountBalanceTo "bob should receive money" bob (> constInitBalance)
    expectAccountBalanceTo "carol should also receive money" carol (> constInitBalance)
    expectZeroTotalLiquidity
    )

tests = hUnitTestToTests $ createTokenTestList
    [ simple1to1ScenarioTest
    , simple1to2ScenarioTest
    ]
