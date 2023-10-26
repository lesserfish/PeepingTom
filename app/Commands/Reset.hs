module Commands.Reset (resetCommand) where

import Commands.Base
import qualified Data.Map as Map
import qualified PeepingTom.State as PTState
import State
import Text.Printf (printf)

resetAction :: [String] -> State -> IO State
resetAction args state = do
    if length args > 0
        then do
            putStrLn $ "reset does not require any arguments"
            putStrLn $ resetHelp
            return state
        else do
            let name = sCurrentState state
            let new_map = Map.delete name (sStates state)
            let new_state = state{sStates = new_map}
            return new_state

resetHelp :: String
resetHelp = "\nreset: Resets the current list"

resetCommand =
    ACommand
        { cName = "reset"
        , action = resetAction
        , help = resetHelp
        }
