import           System.Environment                    (getArgs)

import           Money.Superfluid.Validator.Simulation (runSimMonad)

import qualified Money.Superfluid.Validator.Demos.DFA
import qualified Money.Superfluid.Validator.Demos.Expo

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--demo", "expo"] -> runSimMonad Money.Superfluid.Validator.Demos.Expo.demo
        ["--demo", "dfa"]  -> runSimMonad Money.Superfluid.Validator.Demos.DFA.demo
        _                  -> putStrLn "---- Welcome to Superfluid Validator ----"
