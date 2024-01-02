module Commands.Update (updateCommand) where

import Commands.Base
import qualified Data.Map as Map
import qualified PeepingTom.Scan as PTScan
import qualified PeepingTom.State as PTState
import State
import Text.Printf (printf)

updateAction :: [String] -> State -> IO State
updateAction args state = do
    if length args > 0
        then do
            putStrLn $ "update does not require any arguments."
            return state
        else do
            let stateName = sCurrentState state
            let mptstate = Map.lookup stateName (sStates state)
            case mptstate of
                Nothing -> do
                    putStrLn $ "State has not been initialized yet. Perform an initial scan before attempting to update it."
                    return state
                Just ptstate -> do
                    let stopsig = (oSendStopSig . sOptions $ state)
                    let chunk_size = (oChunkSize . sOptions $ state)
                    let options = PTScan.ScanOptions chunk_size stopsig

                    ptstate' <- PTScan.updateStateS options ptstate
                    putStrLn $ PTState.showState 5 5 ptstate'
                    let newmap = Map.adjust (\_ -> ptstate') stateName (sStates state) :: PTMap
                    let new_state = state{sStates = newmap}
                    return new_state

updateHelp :: String
updateHelp = "\nupdate: Updates the values of the current candidates.\n\nUsage:\n\tupdate"

updateCommand =
    ACommand
        { cName = "update"
        , action = updateAction
        , help = updateHelp
        }
