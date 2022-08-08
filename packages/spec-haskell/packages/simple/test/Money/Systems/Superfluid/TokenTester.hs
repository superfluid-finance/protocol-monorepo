module Money.Systems.Superfluid.TokenTester where

import           Math.Extras.Double
import           Test.Hspec
import           Test.HUnit

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Default

import qualified Money.Systems.Superfluid.Instances.Simple.System as SF


constInitBalance :: SF.Wad
constInitBalance = SF.toWad (100.0 :: Double)

cZeroWad :: SF.Wad
cZeroWad = SF.toWad (0 :: Double)

-- =====================================================================================================================
-- * TokenTester TestCase Creator
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

createTokenTestCase :: TokenTestCase -> SpecWith ()
createTokenTestCase (TokenTestCase spec runner) = it (testCaseLabel spec) $ do
    evalStateT (do
        let addresses = testAddressesToInit spec
        mapM_ (`createTestAccount` testAccountInitBalance spec) addresses
        runner TokenTestContext { testSpec = spec, testAddresses = addresses }
        ) TokenTesterData
        { sfSys = SF.SimpleSystemData { SF.currentTime = 0 }
        , token = def
        }

createTokenTestSuite :: String -> [TokenTestCase] -> SpecWith ()
createTokenTestSuite name tests = describe name $ mapM_ createTokenTestCase tests

-- =====================================================================================================================
-- * TokenTester Operations
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

-- =====================================================================================================================
-- * TokenTester Assertions
--

expectAccountBalanceTo :: HasCallStack => SF.SimpleAddress -> (SF.Wad -> Assertion) -> TokenTester ()
expectAccountBalanceTo addr expr = do
    balance <- runToken $ SF.balanceOfAccount addr
    liftIO $ expr $ SF.netValueOfRTB balance

expectZeroTotalValueFuzzily :: HasCallStack => Double -> TokenTester ()
expectZeroTotalValueFuzzily tolerance = do
    t <- runToken SF.getCurrentTime
    accounts <- runToken SF.listAccounts
    let (SF.Wad totalBalance) = SF.netValueOfRTB $ SF.sumBalancesAt (map snd accounts) t
    liftIO $ assertBool "Zero Value Invariance" $ fuzzyEq tolerance (fromInteger totalBalance) (0 :: Double)

expectZeroTotalValue :: TokenTester ()
expectZeroTotalValue = expectZeroTotalValueFuzzily 0

-- Convenient utilities just for formatting prettiness (message in the end)

assertBoolWith :: (a -> Bool) -> String -> a -> Assertion
assertBoolWith f msg x = assertBool msg (f x)

assertEqualWith :: (Show a, Eq a) => a -> String -> a -> Assertion
assertEqualWith a msg = assertEqual msg a
