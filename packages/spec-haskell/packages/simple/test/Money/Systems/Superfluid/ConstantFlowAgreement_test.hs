{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.ConstantFlowAgreement_test (tests) where

import           Control.Monad.IO.Class
import           Data.Default
import           Data.Maybe                                                        (fromMaybe)
import           Lens.Micro
import           Test.Hspec                                                        (HasCallStack)
import           Test.HUnit

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement         as CFA
import qualified Money.Systems.Superfluid.Agreements.Indexes.UniversalIndex        as UIDX
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow as CFMUD
--
import qualified Money.Systems.Superfluid.Instances.Simple.System                  as SF
--
import           Money.Systems.Superfluid.TokenTester

-- =====================================================================================================================
-- * Testing Utilities
--

expectNetFlowRateTo :: HasCallStack => SF.SimpleAddress -> (SF.Wad -> Assertion) -> TokenTester ()
expectNetFlowRateTo addr expr = do
    acc <- runToken $ SF.getAccount addr
    liftIO $ expr $ acc ^. (SF.universalData . UIDX.cfa_lenses . CFMUD.netFlowRate)

expectFlowRateTo :: HasCallStack => (SF.SimpleAddress, SF.SimpleAddress) -> (SF.Wad -> Assertion) -> TokenTester ()
expectFlowRateTo (sender, receiver) expr = do
    flow <- runToken $ fromMaybe def <$> SF.viewFlow (CFA.OperationPartiesF sender receiver)
    liftIO $ expr $ CFA.flow_rate flow

tstep = 1000 :: SF.SimpleTimestamp

-- 1x test unit for flow rate
fr1x = SF.toWad(0.001 :: Double)

u1x = fromIntegral tstep * fr1x


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

    -- T1: creating flow: alice -> bob 1x
    runToken $ SF.updateFlow (CFA.OperationPartiesF alice bob) fr1x
    expectFlowRateTo  (alice,   bob) $ assertEqualWith fr1x
    expectFlowRateTo  (alice, carol) $ assertEqualWith    0
    expectNetFlowRateTo alice $ assertEqualWith (-fr1x)
    expectNetFlowRateTo bob   $ assertEqualWith   fr1x
    expectNetFlowRateTo carol $ assertEqualWith      0
    expectZeroTotalValue

    -- T2: move time forward and test balance moves
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqualWith  constInitBalance
    expectZeroTotalValue

    -- T2a: updating flow: alice -> bob 2x
    runToken $ SF.updateFlow (CFA.OperationPartiesF alice bob) (2*fr1x)
    expectFlowRateTo  (alice,   bob) $ assertEqualWith (2*fr1x)
    expectFlowRateTo  (alice, carol) $ assertEqualWith       0
    expectNetFlowRateTo alice $ assertEqualWith (- 2*fr1x)
    expectNetFlowRateTo bob   $ assertEqualWith (  2*fr1x)
    expectNetFlowRateTo carol $ assertEqualWith      0
    expectZeroTotalValue

    -- T3: move time forward and test balance moves
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 3*u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + 3*u1x)
    expectAccountBalanceTo carol $ assertEqualWith  constInitBalance
    expectZeroTotalValue
    )

simple1to2ScenarioTest = TokenTestCase TokenTestSpec
    { testCaseLabel = "Simple 1to2 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    -- T0: test initial condition
    let [alice, bob, carol] = testAddresses ctx
    expectZeroTotalValue

    -- T1: creating flow: alice -> bob 1x
    runToken $ SF.updateFlow (CFA.OperationPartiesF alice bob) fr1x
    expectFlowRateTo (alice, bob)   $ assertEqualWith fr1x
    expectFlowRateTo (alice, carol) $ assertEqualWith    0
    expectFlowRateTo (bob, carol)   $ assertEqualWith    0
    expectNetFlowRateTo alice $ assertEqualWith (-fr1x)
    expectNetFlowRateTo bob   $ assertEqualWith   fr1x
    expectNetFlowRateTo carol $ assertEqualWith      0
    expectZeroTotalValue

    -- T2: move time forward and test balance moves
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + u1x)
    expectAccountBalanceTo carol $ assertEqualWith  constInitBalance
    expectZeroTotalValue

    -- T2a: creating flow: alice -> carol 2x
    runToken $ SF.updateFlow (CFA.OperationPartiesF alice carol) (3*fr1x)
    expectFlowRateTo (alice, bob)   $ assertEqualWith    fr1x
    expectFlowRateTo (alice, carol) $ assertEqualWith (3*fr1x)
    expectFlowRateTo (bob, carol)   $ assertEqualWith       0
    expectNetFlowRateTo alice $ assertEqualWith  (-4*fr1x)
    expectNetFlowRateTo bob   $ assertEqualWith      fr1x
    expectNetFlowRateTo carol $ assertEqualWith  ( 3*fr1x)
    expectZeroTotalValue

    -- T3: move time forward and test balance moves
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 5 * u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance + 2 * u1x)
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance + 3 * u1x)
    expectZeroTotalValue

    -- T3a: updating flow: alice -> bob 2x
    runToken $ SF.updateFlow (CFA.OperationPartiesF alice bob) (2*fr1x)
    expectFlowRateTo (alice, bob)   $ assertEqualWith (2*fr1x)
    expectFlowRateTo (alice, carol) $ assertEqualWith (3*fr1x)
    expectFlowRateTo (bob, carol)   $ assertEqualWith       0
    expectNetFlowRateTo alice $ assertEqualWith  (-5*fr1x)
    expectNetFlowRateTo bob   $ assertEqualWith  ( 2*fr1x)
    expectNetFlowRateTo carol $ assertEqualWith  ( 3*fr1x)
    expectZeroTotalValue

    -- T4: move time forward and test balance moves
    timeTravel tstep
    expectAccountBalanceTo alice $ assertEqualWith (constInitBalance - 10 * u1x)
    expectAccountBalanceTo bob   $ assertEqualWith (constInitBalance +  4 * u1x)
    expectAccountBalanceTo carol $ assertEqualWith (constInitBalance +  6 * u1x)
    expectZeroTotalValue
      )

tests = createTokenTestSuite "ConstantFlowAgreement System Testsuite"
    [ simple1to1ScenarioTest
    , simple1to2ScenarioTest
    ]
