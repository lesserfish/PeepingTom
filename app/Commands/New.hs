module Commands.New (newCommand) where

import Commands.Base
import qualified PeepingTom.State as PTState
import State
import Text.Printf

newAction :: [String] -> State -> IO State
newAction args state = do
    let maybename = getString args
    case maybename of
        Nothing -> do
            putStrLn $ printf "Could not understand the argument: %s" (show args)
            return state
        Just name -> do
            if length name == 0
                then do
                    putStrLn $ printf "A name need to be specified"
                    putStrLn $ newHelp
                    return state
                else do
                    let new_state = state{sCurrentState = name}
                    return new_state
newHelp :: String
newHelp = "\nnew: Creates a new empty state with a given name.\n\nUsage:\n\n\tnew [name]\n\n"

newCommand =
    ACommand
        { cName = "new"
        , action = newAction
        , help = newHelp
        }
