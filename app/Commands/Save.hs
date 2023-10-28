module Commands.Save (saveCommand) where

import Commands.Base
import qualified Data.Map as Map
import qualified PeepingTom.State as PTState
import State
import Text.Printf (printf)

saveAction :: [String] -> State -> IO State
saveAction args state = do
    let maybename = getString args
    case maybename of
        Nothing -> do
            putStrLn $ printf "Could not understand the argument: %s" (show args)
            return state
        Just name -> do
            if length name == 0
                then do
                    putStrLn $ printf "A name need to be specified"
                    putStrLn $ saveHelp
                    return state
                else do
                    let maybePTState = Map.lookup (sCurrentState state) (sStates state) :: Maybe PTState.PeepState
                    case maybePTState of
                        Nothing -> do
                            putStrLn $ printf "Current state is not initialized. Not saving."
                            return state
                        Just ptstate -> do
                            let newmap = Map.insert name ptstate (sStates state) :: PTMap
                            let new_state = state{sStates = newmap}
                            return new_state

saveHelp :: String
saveHelp = "\nsave: Saves current state to memory.\n\nUsage:\n\tsave [name]"

saveCommand =
    ACommand
        { cName = "save"
        , action = saveAction
        , help = saveHelp
        }
