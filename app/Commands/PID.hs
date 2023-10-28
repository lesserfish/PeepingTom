module Commands.PID (pidCommand) where

import Commands.Base
import qualified Data.Map as Map
import qualified PeepingTom.State as PTState
import State
import Text.Printf (printf)

pidAction :: [String] -> State -> IO State
pidAction args state = do
    if length args /= 1
        then do
            putStrLn $ "pid expects a PID value as argument. "
            putStrLn $ pidHelp
            return state
        else do
            let pidstr = args !! 0
            let pid = read pidstr :: Int
            let new_state = state{sPID = pid}
            return new_state
pidHelp :: String
pidHelp = "\npid: Sets the PID of the process on which initial scans are going to execute.\n\nUsage:\n\npid [pid]\n\n"

pidCommand :: Command
pidCommand =
    ACommand
        { cName = "pid"
        , action = pidAction
        , help = pidHelp
        }
