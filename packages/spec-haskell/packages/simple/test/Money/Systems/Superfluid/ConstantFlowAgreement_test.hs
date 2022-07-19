{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.ConstantFlowAgreement_test (tests) where

import           Control.Monad.IO.Class
import           Data.Default
import           Data.Maybe                                                (fromMaybe)
import           Lens.Micro
import           Test.Hspec                                                (HasCallStack)
import           Test.HUnit

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement as CFA
--
import qualified Money.Systems.Superfluid.Instances.Simple.System          as SF
--
import           Money.Systems.Superfluid.TokenTester

-- =====================================================================================================================
-- * Testing Utilities
--

expectNetFlowRateTo :: HasCallStack
    => SF.SimpleAddress -> (SF.Wad -> Bool) -> String -> TokenTester ()
expectNetFlowRateTo addr expr label = do
    acc <- runToken $ SF.getAccount addr
    liftIO $ assertBool label $ expr $ acc^.SF.cfaMonetaryUnitLens^.CFA.netFlowRate

expectFlowRateTo :: HasCallStack
    => (SF.SimpleAddress, SF.SimpleAddress) -> (SF.Wad -> Bool) -> String -> TokenTester ()
expectFlowRateTo (sender, receiver) expr label = do
    (CFA.MkContractData flow) <- runToken $ fromMaybe def <$> SF.viewFlow (CFA.ContractPartiesF sender receiver)
    liftIO $ assertBool label $ expr $ flow^.CFA.flowRate

-- 1x test unit for flow rate
u1x = SF.toWad(0.0001 :: Double)

-- =====================================================================================================================
-- * Test Scenarios
--

simple1to1ScenarioTest = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 1to1 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol] = testAddresses ctx
    -- T0: test initial condition
    expectZeroTotalValue
    accounts' <- runToken SF.listAccounts
    liftIO $ assertEqual "expected number of accounts" 4 (length accounts')

    -- T1: test initial condition
    -- creating flow: alice -> bob @ 0.0001/s
    runToken $ SF.updateFlow (CFA.ContractPartiesF alice bob) u1x
    expectNetFlowRateTo alice (== -u1x) "alice should have -1x net flowrate"
    expectNetFlowRateTo bob   (== u1x)  "bob should have 1x net flowrate"
    expectNetFlowRateTo carol (== 0)    "carol should have zero net flowrate"
    expectFlowRateTo  (alice,   bob) (== u1x) "alice -> bob should have 1x flowrate"
    expectFlowRateTo  (alice, carol) (== 0)   "alice -> carol should have zero flowrate"

    -- T2: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo alice (< constInitBalance)  "alice should send money"
    expectAccountBalanceTo bob   (> constInitBalance)  "bob should receive money"
    expectAccountBalanceTo carol (== constInitBalance) "carol should be the same"
    expectZeroTotalValue
    )

simple1to2ScenarioTest = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 1to2 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    -- T0: test initial condition
    let [alice, bob, carol] = testAddresses ctx
    runToken $ SF.updateFlow (CFA.ContractPartiesF alice bob)   u1x
    runToken $ SF.updateFlow (CFA.ContractPartiesF alice carol) (2*u1x)
    expectNetFlowRateTo alice (== -3*u1x) "alice should have -3x net flowrate"
    expectNetFlowRateTo bob   (== u1x)    "bob should have 1x net flowrate"
    expectNetFlowRateTo carol (== 2*u1x)  "carol should have 2x net flowrate"
    expectFlowRateTo (alice, bob)   (== u1x)   "alice -> bob should have 1x flowrate"
    expectFlowRateTo (alice, carol) (== 2*u1x) "alice -> carol should have 2x flowrate"
    expectFlowRateTo (bob, carol)   (== 0)     "bob -> carol should have zero flowrate"

    -- T1: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo alice (< constInitBalance) "alice should send money"
    expectAccountBalanceTo bob   (> constInitBalance)   "bob should receive money"
    expectAccountBalanceTo carol (> constInitBalance) "carol should also receive money"
    expectZeroTotalValue
    )

tests = createTokenTestSuite "ConstantFlowAgreement System Testsuite"
    [ simple1to1ScenarioTest
    , simple1to2ScenarioTest
    ]
