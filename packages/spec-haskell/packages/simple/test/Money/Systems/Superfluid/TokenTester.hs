{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Systems.Superfluid.TokenTester where

import           Test.Hspec
import           Test.HUnit

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Default
import           Data.Maybe                                                (fromMaybe)

import           Lens.Micro

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement as CFA
--
import qualified Money.Systems.Superfluid.Instances.Simple.System          as SF


constInitBalance :: SF.Wad
constInitBalance = SF.toWad (100.0 :: Double)

cZeroWad :: SF.Wad
cZeroWad = SF.toWad (0 :: Double)

-- ============================================================================
-- | TokenTester TestCase Creator
--
type TokenMonad = SF.SimpleTokenStateT IO

data TokenTesterData = TokenTesterData
    { sfSys :: SF.SimpleSystemData
    , token :: SF.SimpleTokenData
    }
type TokenTester = StateT TokenTesterData IO

data TokenTestSpec = TokenTestSpec
    { testCaseLabel          :: String
    , testAddressesToInit    :: [SF.SimpleAddress]
    , testAccountInitBalance :: SF.Wad
    }

data TokenTestContext = TokenTestContext
    { testSpec      :: TokenTestSpec
    , testAddresses :: [SF.SimpleAddress]
    }

data TokenTestCase = TokenTestCase TokenTestSpec (TokenTestContext -> TokenTester ())

createTokenTestCase (TokenTestCase spec runner) = it (testCaseLabel spec) $ do
    evalStateT (do
        let addresses = testAddressesToInit spec
        mapM_ (`createTestAccount` testAccountInitBalance spec) addresses
        runner TokenTestContext { testSpec = spec, testAddresses = addresses }
        ) TokenTesterData
        { sfSys = SF.SimpleSystemData { SF.currentTime = 0 }
        , token = def
        }

createTokenTestSuite name tests = describe name $ mapM_ createTokenTestCase tests

-- ============================================================================
-- | TokenTester Operations
--
timeTravel :: HasCallStack => SF.SimpleTimestamp -> TokenTester ()
timeTravel d = modify (\vs -> vs { sfSys = (sfSys vs) { SF.currentTime = (+ d) . SF.currentTime . sfSys $ vs } })

runToken :: HasCallStack => TokenMonad a -> TokenTester a
runToken m = do
    s <- get
    (a, token') <- liftIO $ SF.runSimpleTokenStateT m (sfSys s) (token s)
    modify (\vs -> vs { token = token' })
    return a

createTestAccount :: HasCallStack => SF.SimpleAddress -> SF.Wad -> TokenTester ()
createTestAccount addr initBalance = runToken $ do
    SF.mintValue addr initBalance
    acc <- SF.getAccount addr
    SF.addAccount addr acc

-- ============================================================================
-- | TokenTester Assertions
--
expectAccountBalanceTo :: HasCallStack => String -> SF.SimpleAddress -> (SF.Wad -> Bool) -> TokenTester ()
expectAccountBalanceTo label addr expr = do
    balance <- runToken $ SF.balanceOfAccount addr
    liftIO $ assertBool label (expr . SF.netValueOfRTB $ balance)

expectZeroTotalValue :: HasCallStack => TokenTester ()
expectZeroTotalValue = do
    t <- runToken SF.getCurrentTime
    accounts <- runToken SF.listAccounts
    liftIO $ assertBool "Zero Value Invariance"
        ((== 0) . SF.netValueOfRTB $ SF.sumBalancesAt (map snd accounts) t)

expectCFANetFlowRateTo :: HasCallStack
    => String -> SF.SimpleAddress -> (SF.Wad -> Bool) -> TokenTester ()
expectCFANetFlowRateTo label addr expr = do
    acc <- runToken $ SF.getAccount addr
    liftIO $ assertBool label $ expr $ acc^.SF.cfaMonetaryUnitLens^.CFA.netFlowRate

expectCFAFlowRateTo :: HasCallStack
    => String -> (SF.SimpleAddress, SF.SimpleAddress) -> (SF.Wad -> Bool) -> TokenTester ()
expectCFAFlowRateTo label (sender, receiver) expr = do
    (CFA.MkContractData flow) <- runToken $ fromMaybe def <$> SF.viewFlow (CFA.ContractPartiesF sender receiver)
    liftIO $ assertBool label $ expr $ flow^.CFA.flowRate
