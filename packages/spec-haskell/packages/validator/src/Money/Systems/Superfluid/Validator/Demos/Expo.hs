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
initBalance = SF.toWad (100.0 :: Double)

alice = "alice"
bob = "bob"
carol = "carol"
f1x = SF.toWad (0.0001::Double)
u1x = SF.toWad (5 :: Double)
day = 3600 * 24

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
    liftIO $ putStrLn "# DAY 1: create flows"
    runToken token $ SF.updateFlow (CFA.OperationPartiesF alice bob)      f1x
    runToken token $ SF.updateFlow (CFA.OperationPartiesF alice carol) (2*f1x)
    runSimTokenOp token printTokenState

    timeTravel day
    t2 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 2: nothing " ++ show (t2 - t1)
    runSimTokenOp token printTokenState

    timeTravel day
    t3 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 3: bob transfer 1x to alice" ++ show (t3 - t2)
    runToken token $ SF.transfer (ITA.OperationPartiesF bob alice) u1x
    runSimTokenOp token printTokenState

    timeTravel day
    t4 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 4: bob subscribes 100 units from alice, and alice distribute 1x" ++ show (t4 - t3)
    runToken token $ SF.updateProportionalDistributionSubscription bob alice 0 100
    runToken token $ SF.distributeProportionally alice 0 u1x
    runSimTokenOp token printTokenState

    timeTravel day
    t5 <- getCurrentTime
    liftIO $ putStrLn $ "# DAY 4: bob subscribes 400 units from alice, and alice distribute 5x" ++ show (t5 - t4)
    runSimTokenOp token printTokenState
    runToken token $ SF.updateProportionalDistributionSubscription carol alice 0 400
    runToken token $ SF.distributeProportionally alice 0 (5*u1x)
    runSimTokenOp token printTokenState
