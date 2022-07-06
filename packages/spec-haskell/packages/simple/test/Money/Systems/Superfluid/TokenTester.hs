module Money.Systems.Superfluid.TokenTester where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Default
import           Data.Maybe                                                (fromMaybe)
import           GHC.Stack
import           Test.HUnit
    ( Test (TestCase, TestLabel, TestList)
    , assertBool
    )

import qualified Money.Systems.Superfluid.Concepts.RealtimeBalance         as RTB
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement as CFA

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
    { testLabel              :: String
    , testAddressesToInit    :: [SF.SimpleAddress]
    , testAccountInitBalance :: SF.Wad
    }

data TokenTestContext = TokenTestContext
    { testSpec      :: TokenTestSpec
    , testAddresses :: [SF.SimpleAddress]
    }

data TokenTestCase = TokenTestCase TokenTestSpec (TokenTestContext -> TokenTester ())

createTokenTestCase :: TokenTestCase -> Test
createTokenTestCase (TokenTestCase spec runner) = TestLabel (testLabel spec) $ TestCase $ do
    evalStateT (do
        let addresses = testAddressesToInit spec
        mapM_ (`createTestAccount` testAccountInitBalance spec) addresses
        runner TokenTestContext { testSpec = spec, testAddresses = addresses }
        ) TokenTesterData
        { sfSys = SF.SimpleSystemData { SF.currentTime = 0 }
        , token = def
        }

createTokenTestList :: [TokenTestCase] -> Test
createTokenTestList = TestList . map createTokenTestCase

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
    SF.mintLiquidity addr initBalance
    acc <- SF.getAccount addr
    SF.addAccount addr acc

-- ============================================================================
-- | TokenTester Assertions
--
expectAccountBalanceTo :: HasCallStack => String -> SF.SimpleAddress -> (SF.Wad -> Bool) -> TokenTester ()
expectAccountBalanceTo label addr expr = do
    balance <- runToken $ SF.balanceOfAccount addr
    liftIO $ assertBool label (expr . RTB.liquidityRequiredForRTB $ balance)

expectZeroTotalLiquidity :: HasCallStack => TokenTester ()
expectZeroTotalLiquidity = do
    t <- runToken SF.getCurrentTime
    accounts <- runToken SF.listAccounts
    liftIO $ assertBool "Zero Liquidity Invariance"
        ((== 0) . RTB.liquidityRequiredForRTB $ SF.sumBalancesAt (map snd accounts) t)

expectCFANetFlowRateTo :: HasCallStack
    => String -> SF.SimpleAddress -> (SF.Wad -> Bool) -> TokenTester ()
expectCFANetFlowRateTo label addr expr = do
    account <- runToken $ SF.getAccount addr
    liftIO $ assertBool label (expr . CFA.netFlowRate . SF.viewCFAAccount $ account)

expectCFAFlowRateTo :: HasCallStack
    => String -> (SF.SimpleAddress, SF.SimpleAddress) -> (SF.Wad -> Bool) -> TokenTester ()
expectCFAFlowRateTo label (sender, receiver) expr = do
    flow <- runToken $ fromMaybe def <$> SF.viewFlow (CFA.ContractPartiesF sender receiver)
    liftIO $ assertBool label (expr . CFA.flowRate $ flow)
