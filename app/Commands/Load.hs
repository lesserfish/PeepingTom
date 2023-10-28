module Commands.Load (loadCommand) where

import Commands.Base
import qualified Data.Map as Map
import qualified PeepingTom.State as PTState
import State
import Text.Printf (printf)

loadAction :: [String] -> State -> IO State
loadAction args state = do
    let maybename = getString args
    case maybename of
        Nothing -> do
            putStrLn $ printf "Could not understand the argument: %s" (show args)
            return state
        Just name -> do
            if length name == 0
                then do
                    putStrLn $ printf "A name need to be specified"
                    putStrLn $ loadHelp
                    return state
                else do
                    let maybePTState = Map.lookup name (sStates state) :: Maybe PTState.PeepState
                    case maybePTState of
                        Nothing -> do
                            putStrLn $ printf "There is no state with the name '%s'" name
                            return state
                        Just _ -> do
                            let new_state = state{sCurrentState = name}
                            return new_state

loadHelp :: String
loadHelp = "\nload: Loads a state from memory.\n\nUsage:\n\tload [name]"

loadCommand =
    ACommand
        { cName = "load"
        , action = loadAction
        , help = loadHelp
        }
