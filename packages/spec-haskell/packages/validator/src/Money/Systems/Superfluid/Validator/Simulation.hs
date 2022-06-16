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
    , printAccount
    , printAccountByAlias
    , printTokenState
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Default
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
getAccountByAlias alias _= SF.getAccount $ fromJust $ SF.createSimpleAddress alias

printAccount :: HasCallStack => SF.SimpleAccount -> SimData -> TokenMonad ()
printAccount acc _ = do
    t <- SF.getCurrentTime
    liftIO $ putStrLn $ SF.showAccountAt acc t ++ "\n"

printAccountByAlias :: HasCallStack => String -> SimData -> TokenMonad ()
printAccountByAlias alias s = getAccountByAlias alias s >>= flip printAccount s

sumTotalLiquidity :: HasCallStack => SimData -> TokenMonad SF.SimpleRealtimeBalance
sumTotalLiquidity _ = do
    t <- SF.getCurrentTime
    accounts <- SF.listAccounts
    return $ SF.sumAccounts (map snd accounts) t

printTokenState :: HasCallStack => SimData -> TokenMonad ()
printTokenState s = do
    let banner = 60 `replicate` '-'
    liftIO $ putStrLn banner
    liftIO $ putStrLn "## Accounts\n"
    accounts <- SF.listAccounts
    -- mapM_ (\x -> printAccount (snd x) s) accounts
    mapM_ (flip (printAccount . snd) s) accounts
    totalLiquidtySum <- sumTotalLiquidity s
    liftIO $ putStrLn "## Token Info\n"
    liftIO $ putStrLn $ "Total Balance: " ++ show totalLiquidtySum
    liftIO $ putStrLn (banner ++ "\n")
