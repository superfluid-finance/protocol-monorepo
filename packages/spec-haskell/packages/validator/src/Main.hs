import           System.Environment                            (getArgs)

import           Money.Systems.Superfluid.Validator.Simulation (runSimMonad)

import qualified Money.Systems.Superfluid.Validator.Demos.DFA  as DFA
import qualified Money.Systems.Superfluid.Validator.Demos.Expo as Expo

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--demo", "expo"] -> runSimMonad Expo.demo
        ["--demo", "dfa"]  -> runSimMonad DFA.demo
        _                  -> putStrLn "---- Welcome to Superfluid Validator ----"
