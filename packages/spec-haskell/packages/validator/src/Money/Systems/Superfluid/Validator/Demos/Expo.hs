{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.Validator.Demos.Expo (demo) where

import           Control.Monad.IO.Class
import           Data.Time.Clock.POSIX                              (getPOSIXTime)
import           GHC.Stack

import qualified Money.Systems.Superfluid.Agreements.UniversalIndex as UIDX
--
import qualified Money.Systems.Superfluid.Instances.Simple.System   as SF
--
import           Money.Systems.Superfluid.Validator.Simulation


now :: IO SF.SimpleTimestamp
now =  do
    t <- getPOSIXTime
    return $ fromIntegral (round t :: SF.SimpleTimestamp)

initBalance :: SF.Wad
initBalance = SF.toWad (100.0 :: Double)

demo :: HasCallStack => SimMonad ()
demo = do
    liftIO $ putStrLn "==== Superfluid Specification Expo ====\n"
    let token = "DAI"
    t0 <- liftIO now
    timeTravel t0
    liftIO $ putStrLn "# T0: create test accounts"
    let alice = "alice"
    let bob = "bob"
    let carol = "carol"
    createToken token [alice, bob, carol] initBalance
    runSimTokenOp token printTokenState

    let t1 = t0
    liftIO $ putStrLn $ "# T1: create flows" ++ show (t1 - t0)
    runToken token $ SF.updateFlow (UIDX.CFAContractPartiesF alice bob) (SF.toWad (0.0001::Double))
    runToken token $ SF.updateFlow (UIDX.CFAContractPartiesF alice carol) (SF.toWad (0.0002::Double))
    runSimTokenOp token printTokenState

    timeTravel $ 3600 * 24
    t2 <- getCurrentTime
    liftIO $ putStrLn $ "# T2: advanced one full day " ++ show (t2 - t0)
    runSimTokenOp token printTokenState
