import           System.Environment                            (getArgs)

import           Money.Systems.Superfluid.Validator.Simulation (runSimMonad)

import qualified Money.Systems.Superfluid.Validator.Demos      as Demos

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--demo", "expo"] -> runSimMonad Demos.expo
        ["--demo", "dfa"]  -> runSimMonad Demos.dfa
        ["--demo", "cfda"] -> runSimMonad Demos.cfda
        _                  -> putStrLn "---- Welcome to Superfluid Validator ----"
