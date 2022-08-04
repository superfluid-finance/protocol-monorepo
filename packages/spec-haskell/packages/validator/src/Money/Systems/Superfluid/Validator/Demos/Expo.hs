{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Money.Systems.Superfluid.Validator.Demos.Expo (demo) where

import           Control.Monad.IO.Class
import           Data.Time.Clock.POSIX                                        (getPOSIXTime)
import           GHC.Stack                                                    (HasCallStack)

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement    as CFA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA
--
import qualified Money.Systems.Superfluid.Instances.Simple.System             as SF
--
import           Money.Systems.Superfluid.Validator.Simulation


now :: IO SF.SimpleTimestamp
now =  do
    t <- getPOSIXTime
    return $ fromIntegral (round t :: SF.SimpleTimestamp)

initBalance :: SF.Wad
initBalance = SF.toWad (10000.0 :: Double)

alice = "alice"
bob = "bob"
carol = "carol"

day = 3600 * 24 :: SF.SimpleTimestamp
fr1x = SF.toWad ((100 :: Double) / fromIntegral day)
u1x = fromIntegral day * fr1x

demo :: HasCallStack => SimMonad ()
demo = do
    liftIO $ putStrLn "==== Superfluid Specification Expo ====\n"
    let token = "DAI"

    t0 <- liftIO now
    timeTravel t0

    liftIO $ putStrLn "# DAY 0: create test accounts"
    createToken token [alice, bob, carol] initBalance
    runSimTokenOp token printTokenState

    let t1 = t0
    liftIO $ putStrLn $ "# DAY 1: create flows @" ++ show t1
    runToken token $ SF.updateFlow (CFA.OperationPartiesF alice bob)      fr1x
    runToken token $ SF.updateFlow (CFA.OperationPartiesF alice carol) (2*fr1x)
    runSimTokenOp token printTokenState

    timeTravel day
    t2 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 2: nothing @" ++ show t2
    runSimTokenOp token printTokenState

    timeTravel day
    t3 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 3: bob transfer 1x to alice @" ++ show t3
    runToken token $ SF.transfer (ITA.OperationPartiesF bob alice) u1x
    runSimTokenOp token printTokenState

    timeTravel day
    t4 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 4: bob subscribes 100 units from alice, and alice distribute 1x @" ++ show t4
    runToken token $ SF.updateProportionalDistributionSubscription bob alice 0 1000
    runToken token $ SF.distributeProportionally alice 0 u1x
    runSimTokenOp token printTokenState

    timeTravel day
    t5 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 5: carol subscribes 400 units from alice, and alice distribute 5x @" ++ show t5
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
