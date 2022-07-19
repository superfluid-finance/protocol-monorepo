{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.DecayingFlowAgreement_test (tests) where

import           Control.Monad.IO.Class
import           Data.Default
import           Data.Maybe                                                (fromMaybe)
import           Lens.Micro
import           Math.Extras.Double                                        (fuzzyEq)
import           Test.Hspec                                                (HasCallStack)
import           Test.HUnit

import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement as DFA
--
import qualified Money.Systems.Superfluid.Instances.Simple.System          as SF
--
import           Money.Systems.Superfluid.TokenTester

-- =====================================================================================================================
-- * Testing Utilities
--

tolerance = 1e5

(~=) = fuzzyEq tolerance

expectΕValTo :: HasCallStack
    => SF.SimpleAddress -> (Double -> Bool) -> String -> TokenTester ()
expectΕValTo addr expr label = do
    acc <- runToken $ SF.getAccount addr
    let εVal = acc^.SF.dfaMonetaryUnitLens^.DFA.εVal
    liftIO $ assertBool label $ expr εVal

expectDistributionLimitTo :: HasCallStack
    => (SF.SimpleAddress, SF.SimpleAddress) -> (SF.Wad -> Bool) -> String -> TokenTester ()
expectDistributionLimitTo (sender, receiver) expr label = do
    (DFA.MkContractData flow) <- runToken $ fromMaybe def <$> SF.viewDecayingFlow (DFA.ContractPartiesF sender receiver)
    liftIO $ assertBool label $ expr $ flow^.DFA.distributionLimit

-- 1x test unit for distribution limit
u1x = SF.toWad (42 :: Double)
u1xd = let (SF.Wad d) = u1x in fromInteger d :: Double

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
    runToken $ SF.updateDecayingFlow (DFA.ContractPartiesF alice bob) u1x
    expectΕValTo alice (== (-u1xd)) "alice should have -1x net distribution limit"
    expectΕValTo bob   (== u1xd)    "bob should have 1x net distribution limit"
    expectΕValTo carol (== 0)       "carol should have zero net distribution limit"
    expectDistributionLimitTo (alice, bob)   (== u1x)  "alice -> bob should have 1x distribution limit"
    expectDistributionLimitTo (alice, carol) (== 0)  "alice -> carol should have zero distribution limit"

    -- T2: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo alice (< constInitBalance) "alice should send money"
    expectAccountBalanceTo bob   (> constInitBalance) "bob should receive money"
    expectAccountBalanceTo carol (== constInitBalance) "carol should be the same"
    expectZeroTotalValueFuzzily 0 -- 0 tolerance
      )

simple1to2ScenarioTest = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 1to2 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    -- T0: test initial condition
    let [alice, bob, carol] = testAddresses ctx
    runToken $ SF.updateDecayingFlow (DFA.ContractPartiesF alice bob)   u1x
    runToken $ SF.updateDecayingFlow (DFA.ContractPartiesF alice carol) (u1x * 2)
    expectΕValTo alice (~= (-3*u1xd)) "alice should have -3x net distribution limit"
    expectΕValTo bob   (~= u1xd)      "bob should have 1x net distribution limit"
    expectΕValTo carol (~= (2*u1xd))  "carol should have 1x net distribution limit"
    expectDistributionLimitTo (alice, bob)   (== u1x)   "alice -> bob should have 1x distribution limit"
    expectDistributionLimitTo (alice, carol) (== 2*u1x) "alice -> carol should have zero distribution limit"

    -- T1: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo alice (< constInitBalance) "alice should send money"
    expectAccountBalanceTo bob   (> constInitBalance) "bob should receive money"
    expectAccountBalanceTo carol (> constInitBalance) "carol should also receive money"
    expectZeroTotalValueFuzzily tolerance
    )

simpleLoopScenarioTest = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple Loop Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    -- T0: test initial condition
    let [alice, bob, carol] = testAddresses ctx
    runToken $ SF.updateDecayingFlow (DFA.ContractPartiesF alice bob)   u1x
    runToken $ SF.updateDecayingFlow (DFA.ContractPartiesF bob   carol) u1x
    runToken $ SF.updateDecayingFlow (DFA.ContractPartiesF carol alice) u1x
    expectΕValTo alice (== 0) "alice should have zero net distribution limit"
    expectΕValTo bob   (== 0) "bob should have zero net distribution limit"
    expectΕValTo carol (== 0) "carol should have zero net distribution limit"
    expectDistributionLimitTo (alice, bob)   (== u1x) "alice -> bob should have 1x distribution limit"
    expectDistributionLimitTo (bob,   carol) (== u1x) "bob -> alice should 1x distribution limit"
    expectDistributionLimitTo (carol, alice) (== u1x) "carol -> alice should have 1x distribution limit"

    -- T1: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo alice (== constInitBalance) "alice should send money"
    expectAccountBalanceTo bob   (== constInitBalance) "bob should receive money"
    expectAccountBalanceTo carol (== constInitBalance) "carol should also receive money"
    expectZeroTotalValueFuzzily 0 -- 0 tolerance
    )

tests = createTokenTestSuite "DecayingFlowAgreement System Testsuite"
    [ simple1to1ScenarioTest
    , simple1to2ScenarioTest
    , simpleLoopScenarioTest
    ]
