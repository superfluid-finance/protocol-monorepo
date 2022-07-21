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

expectNetFlowRateTo :: HasCallStack => SF.SimpleAddress -> (SF.Wad -> Assertion) -> TokenTester ()
expectNetFlowRateTo addr expr = do
    acc <- runToken $ SF.getAccount addr
    liftIO $ expr $ acc^.SF.universalIndex^. CFA.netFlowRate

expectFlowRateTo :: HasCallStack => (SF.SimpleAddress, SF.SimpleAddress) -> (SF.Wad -> Assertion) -> TokenTester ()
expectFlowRateTo (sender, receiver) expr = do
    (CFA.MkContractData flow) <- runToken $ fromMaybe def <$> SF.viewFlow (CFA.ContractPartiesF sender receiver)
    liftIO $ expr $ flow^.CFA.flowRate

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
    expectNetFlowRateTo alice $ assertEqualWith (-u1x) "alice should have -1x net flowrate"
    expectNetFlowRateTo bob   $ assertEqualWith   u1x  "bob should have 1x net flowrate"
    expectNetFlowRateTo carol $ assertEqualWith     0  "carol should have zero net flowrate"
    expectFlowRateTo  (alice,   bob) $ assertEqualWith u1x "alice -> bob should have 1x flowrate"
    expectFlowRateTo  (alice, carol) $ assertEqualWith   0 "alice -> carol should have zero flowrate"

    -- T2: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo alice $ assertBoolWith (< constInitBalance)  "alice should send money"
    expectAccountBalanceTo bob   $ assertBoolWith (> constInitBalance)  "bob should receive money"
    expectAccountBalanceTo carol $ assertBoolWith (== constInitBalance) "carol should be the same"
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
    expectNetFlowRateTo alice $ assertEqualWith (-3*u1x) "alice should have -3x net flowrate"
    expectNetFlowRateTo bob   $ assertEqualWith     u1x  "bob should have 1x net flowrate"
    expectNetFlowRateTo carol $ assertEqualWith  (2*u1x) "carol should have 2x net flowrate"
    expectFlowRateTo (alice, bob)   $ assertEqualWith    u1x  "alice -> bob should have 1x flowrate"
    expectFlowRateTo (alice, carol) $ assertEqualWith (2*u1x) "alice -> carol should have 2x flowrate"
    expectFlowRateTo (bob, carol)   $ assertEqualWith      0  "bob -> carol should have zero flowrate"

    -- T1: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo alice $ assertBoolWith (< constInitBalance) "alice should send money"
    expectAccountBalanceTo bob   $ assertBoolWith (> constInitBalance) "bob should receive money"
    expectAccountBalanceTo carol $ assertBoolWith (> constInitBalance) "carol should also receive money"
    expectZeroTotalValue
    )

tests = createTokenTestSuite "ConstantFlowAgreement System Testsuite"
    [ simple1to1ScenarioTest
    , simple1to2ScenarioTest
    ]
