module Main where

import CLI
import Commands
import Control.Monad.IO.Class
import System.Console.Haskeline
import System.Console.Haskeline.IO
import System.Exit
import System.IO
import Text.Printf

mySettings :: [Command] -> Settings IO
mySettings cmd =
    Settings
        { historyFile = Nothing
        , complete = completeWordWithPrev Nothing [' '] (autocomplete cmd)
        , autoAddHistory = True
        }

commands :: [Command]
commands = [listCommand, cmdCommand]

defaultState :: State
defaultState = State

run :: State -> IO ()
run initState = runInputT (mySettings commands) (loop initState)
  where
    loop :: State -> InputT IO ()
    loop state = do
        minput <- getInputLine "(0) > "
        case minput of
            Nothing -> return ()
            Just "exit" -> return ()
            Just input -> do
                let maybeaction = getAction (words input) commands
                case maybeaction of
                    Left (ErrorMsg msg) -> do
                        outputStrLn $ printf "Error: %s" msg
                        loop state
                    Right (args, act) -> do
                        state' <- liftIO $ act args state
                        loop state'

main :: IO ()
main = do
    -- create initial state from args passed to main
    run defaultState -- run with initial state
    return ()
