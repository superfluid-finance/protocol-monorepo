{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.Validator.Demos.DFA (demo) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Time.Clock.POSIX                            (getPOSIXTime)
import           GHC.Stack
import           Text.Printf

import qualified Money.Systems.Superfluid.Instances.Simple.System as SF
--
import           Money.Systems.Superfluid.Validator.Simulation

now =  do
    t <- getPOSIXTime
    return $ fromIntegral (round t :: SF.SimpleTimestamp)

initBalance = SF.toWad (10000.0 :: Double)

distributionLimitN n = SF.toWad $ 1000 * (n :: Double)

token = "MAGIC"
alice = "alice"
bob = "bob"
carol = "carol"
dan = "dan"

travelDaysAndPrintBalances :: HasCallStack => SF.SimpleTimestamp -> Int -> SimMonad ()
travelDaysAndPrintBalances t0 days = do
    t <- getCurrentTime
    let t_offset = t - t0
    forM_ [(1::Int) .. 24 * days] $ \i -> do
        timeTravel 3600
        aliceBalance <- runToken token $ SF.balanceOfAccount alice
        bobBalance <- runToken token $ SF.balanceOfAccount bob
        carolBalance <- runToken token $ SF.balanceOfAccount carol
        danBalance <- runToken token $ SF.balanceOfAccount dan
        liftIO $ putStrLn $ printf "%f %s %s %s %s"
            ((fromIntegral (coerce t_offset + i * 3600) :: Double) / (3600.0 * 24))
            (show . SF.untappedLiquidityFromRTB $ aliceBalance)
            (show . SF.untappedLiquidityFromRTB $ bobBalance)
            (show . SF.untappedLiquidityFromRTB $ carolBalance)
            (show . SF.untappedLiquidityFromRTB $ danBalance)

demo :: HasCallStack => SimMonad ()
demo = do
    liftIO $ putStrLn "==== Decaying ConstantFlow Agreement Demo ====\n"
    t0 <- liftIO now
    timeTravel t0
    createToken token [alice, bob, carol, dan] initBalance
    runToken token $ SF.updateDecayingFlow alice bob (distributionLimitN 1)
    travelDaysAndPrintBalances t0 7

    runToken token $ SF.updateDecayingFlow alice carol (distributionLimitN 1)
    travelDaysAndPrintBalances t0 7

    runToken token $ SF.updateDecayingFlow dan alice (distributionLimitN 2)
    travelDaysAndPrintBalances t0 60
