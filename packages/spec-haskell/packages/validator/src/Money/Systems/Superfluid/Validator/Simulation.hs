{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Systems.Superfluid.Validator.Simulation
    -- SimMonad operations
    ( SimMonad
    , runSimMonad
    , getCurrentTime
    , timeTravel
    , runToken
    , runSimTokenOp
    , createToken
    -- TokenMonad operations
    , TokenMonad
    , getAccountByAlias
    , printAccountByAlias
    , printTokenState
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Coerce
import           Data.Default
import           Data.Function                                    ((&))
import           Data.Functor
import qualified Data.Map                                         as M
import           Data.Maybe
import           GHC.Stack

import qualified Money.Systems.Superfluid.Instances.Simple.System as SF

-- ============================================================================
-- | Simulation Monad Stacks
--
--   SimMonad    : SimState | IO
--   TokenMonad  : SimpleTokenState | IO
data SimData = SimData
    { sfSys  :: SF.SimpleSystemData
    , tokens :: M.Map String SF.SimpleTokenData
    }
type SimMonad = StateT SimData IO

type TokenMonad = SF.SimpleTokenStateT IO

runSimMonad :: SimMonad () -> IO ()
runSimMonad = flip evalStateT SimData
    { sfSys = SF.SimpleSystemData { SF.currentTime = 0}
    , tokens = def
    }

-- ============================================================================
-- | SimMonad Operations
--
getCurrentTime :: HasCallStack => SimMonad SF.SimpleTimestamp
getCurrentTime = get <&> SF.currentTime . sfSys

timeTravel :: HasCallStack => SF.SimpleTimestamp -> SimMonad ()
timeTravel d = modify (\vs -> vs { sfSys = (sfSys vs) { SF.currentTime = (+ d) . SF.currentTime . sfSys $ vs } })

runSimTokenOp :: HasCallStack => String -> (SimData -> TokenMonad a) -> SimMonad a
runSimTokenOp tokenId mf = do
    s <- get
    let token' = M.findWithDefault def tokenId (tokens s)
    (a, token'') <- liftIO $ SF.runSimpleTokenStateT (mf s) (sfSys s) token'
    modify (\vs -> vs { tokens = M.insert tokenId token'' (tokens s) })
    return a

runToken :: HasCallStack => String -> TokenMonad a -> SimMonad a
runToken tokenId m = runSimTokenOp tokenId (const m)

createToken :: HasCallStack => String -> [SF.SimpleAddress] -> SF.Wad -> SimMonad ()
createToken tokenId alist initBalance = runToken tokenId $ SF.initSimpleToken alist initBalance

-- ============================================================================
-- | Sim Operations
--
getAccountByAlias :: HasCallStack => String -> SimData -> TokenMonad SF.SimpleAccount
getAccountByAlias alias _ = SF.getAccount $ fromJust $ SF.createSimpleAddress alias

printAccountByAlias :: HasCallStack => String -> SimData -> TokenMonad ()
printAccountByAlias alias s = do
    t <- SF.getCurrentTime
    acc <- getAccountByAlias alias s
    liftIO $ print_account t (alias, acc)

sumTotalValue :: HasCallStack => SimData -> TokenMonad SF.SimpleRealTimeBalance
sumTotalValue _ = do
    t <- SF.getCurrentTime
    accounts <- SF.listAccounts
    return $ SF.sumBalancesAt (map snd accounts) t

printTokenState :: HasCallStack => SimData -> TokenMonad ()
printTokenState s = do
      t <- SF.getCurrentTime
      accounts <- SF.listAccounts
      cfaContracts <- SF.listCFAContracts
      dfaContracts <- SF.listDFAContracts
      distributionContracts <- SF.listDistributionContracts
      subscriptionContracts <- SF.listSubscriptionContracts
      totalLiquidtySum <- sumTotalValue s
      liftIO $ do
          let banner = 60 `replicate` '-'
          putStrLn banner

          putStrLn "## CFA contracts\n"
          mapM_ print_contract cfaContracts
          putStrLn ""

          putStrLn "## DFA Contracts\n"
          mapM_ print_contract dfaContracts
          putStrLn ""

          putStrLn "## PD Distribution Contracts\n"
          mapM_ print_contract distributionContracts
          putStrLn ""

          putStrLn "## Subscription Contracts\n"
          mapM_ print_contract subscriptionContracts
          putStrLn ""

          putStrLn "## Accounts\n"
          mapM_ (\(addr, acc) -> print_account t (coerce addr, acc)) accounts
          putStrLn ""

          putStrLn "## Token Info\n"
          putStrLn $ "Total Balance: " ++ show totalLiquidtySum
          putStrLn banner
          putStrLn ""

print_account t (alias, acc) =
    putStr $ "Account @" ++ alias ++ "\n" ++ accountDetails
    where accountDetails = lines (SF.showAccountAt acc t) & map ("  " ++) & unlines

print_contract (acAddr, ac) =
    putStr $ show acAddr ++ "\n" ++ acDetails
    where acDetails = lines (show ac) & map ("  " ++) & unlines
