{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.Validator.Demos
    ( expo
    , dfa
    , cfda
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.List
import           Data.Time.Clock.POSIX                            (getPOSIXTime)
import           GHC.Stack                                        (HasCallStack)
import           Lens.Micro
import           Text.Printf                                      (printf)

import qualified Money.Systems.Superfluid.Instances.Simple.System as SF
--
import           Money.Systems.Superfluid.Validator.Simulation


now :: IO SF.SimpleTimestamp
now =  do
    t <- getPOSIXTime
    return $ fromIntegral (round t :: SF.SimpleTimestamp)

initBalance :: SF.Wad
initBalance = SF.toWad (10000.0 :: Double)

token = "MAGIC"
alice = "alice"
bob = "bob"
carol = "carol"
dan = "dan"

day = 3600 * 24 :: SF.SimpleTimestamp
fr1x = SF.toWad ((100 :: Double) / fromIntegral day)
u1x = fromIntegral day * fr1x

travelDaysAndPrintBalances t0 accounts days = do
    t <- getCurrentTime
    let t_offset = t - t0
    forM_ [(1::Int) .. 24 * days] $ \i -> do
        timeTravel 3600
        balances <- mapM (runToken token . SF.balanceOfAccount) accounts
        liftIO $ putStrLn $
            printf "%f " ((fromIntegral (coerce t_offset + i * 3600) :: Double) / (3600.0 * 24))
            <>
            unwords (map (\acc -> show $ acc^.SF.untappedValueL) balances)

printSelfGnuplotHeader accounts = putStrLn $
    "plot '__GNUPLOT_FILE__' every ::1 " <>
    intercalate ",'' " (
    zipWith
        (\idx acc -> printf "using 1:%d with lines title '%s'" (idx + 1) (show acc))
        [1..length accounts]
        accounts
    )

expo :: HasCallStack => SimMonad ()
expo = do
    liftIO $ putStrLn "==== Superfluid Specification Expo ====\n"

    t0 <- liftIO now
    timeTravel t0

    liftIO $ putStrLn "# DAY 0: create test accounts"
    createToken token [alice, bob, carol, dan] initBalance
    runSimTokenOp token printTokenState

    let t1 = t0
    liftIO $ putStrLn $ "# DAY 1: create flows @" ++ show t1
    runToken token $ SF.updateFlow alice bob      fr1x
    runToken token $ SF.updateFlow alice carol (2*fr1x)
    runSimTokenOp token printTokenState

    timeTravel day
    t2 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 2: nothing @" ++ show t2
    runSimTokenOp token printTokenState

    timeTravel day
    t3 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 3: bob transfer 1x to alice @" ++ show t3
    runToken token $ SF.transfer bob alice u1x
    runSimTokenOp token printTokenState

    timeTravel day
    t4 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 4: bob subscribes 100 units from alice, and alice distribute 1x to it@" ++ show t4
    runToken token $ SF.updateProportionalDistributionSubscription bob alice 0 1000
    runToken token $ SF.distributeProportionally alice 0 u1x
    runSimTokenOp token printTokenState

    timeTravel day
    t5 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 5: carol subscribes 400 units from alice, and alice distribute 5x to it@" ++ show t5
    runSimTokenOp token printTokenState
    runToken token $ SF.updateProportionalDistributionSubscription carol alice 0 4000
    runToken token $ SF.distributeProportionally alice 0 (5*u1x)
    runSimTokenOp token printTokenState

    timeTravel day
    t6 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 6: alice distribute 1x flow rate @" ++ show t6
    runToken token $ SF.distributeFlow alice 0 fr1x
    runSimTokenOp token printTokenState

    timeTravel day
    t7 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 7: print token state @" ++ show t7
    runSimTokenOp token printTokenState

    timeTravel day
    t8 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 8: dan subscribes 1000 units from alice, annd alice distribute 2x to it" ++ show t8
    runToken token $ SF.updateProportionalDistributionSubscription dan alice 1 1000
    runToken token $ SF.distributeFlow alice 1 (2*fr1x)
    runSimTokenOp token printTokenState

    timeTravel day
    t9 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 9: print token state @" ++ show t9
    runSimTokenOp token printTokenState


dfa :: HasCallStack => SimMonad ()
dfa = do
    let accounts = [alice, bob, carol, dan]
    liftIO $ printSelfGnuplotHeader accounts
    t0 <- liftIO now
    timeTravel t0
    createToken token accounts initBalance

    runToken token $ SF.updateDecayingFlow alice bob u1x
    travelDaysAndPrintBalances t0 accounts 7

    runToken token $ SF.updateDecayingFlow alice carol u1x
    travelDaysAndPrintBalances t0 accounts 7

    runToken token $ SF.updateDecayingFlow dan alice (u1x * 2)
    travelDaysAndPrintBalances t0 accounts 60

cfda :: HasCallStack => SimMonad ()
cfda = do
    let accounts = [alice, bob, carol]
    liftIO $ printSelfGnuplotHeader accounts
    t0 <- liftIO now
    timeTravel t0
    createToken token accounts initBalance

    runToken token $ SF.updateProportionalDistributionSubscription bob alice 0 1000
    runToken token $ SF.distributeFlow alice 0 fr1x
    travelDaysAndPrintBalances t0 accounts 7

    runToken token $ SF.updateProportionalDistributionSubscription carol alice 0 4000
    travelDaysAndPrintBalances t0 accounts 7

    runToken token $ SF.distributeFlow alice 0 (5*fr1x)
    travelDaysAndPrintBalances t0 accounts 7

    runToken token $ SF.distributeFlow alice 0 (2*fr1x)
    travelDaysAndPrintBalances t0 accounts 7
