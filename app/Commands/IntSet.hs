module Commands.IntSet (intSetCommand) where

import Commands.Base
import qualified Data.Map as Map
import qualified PeepingTom.Scan as PTScan
import qualified PeepingTom.State as PTState
import qualified PeepingTom.Writer as PTWriter
import State

intSetAction :: [String] -> State -> IO State
intSetAction args state = do
    if length args /= 1
        then do
            putStrLn $ "intSet requires one integer input"
            putStrLn $ intSetHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let writer = PTWriter.writeInt num
            let stateName = sCurrentState state
            let mptstate = Map.lookup stateName (sStates state)
            case mptstate of
                Nothing -> do
                    putStrLn $ "State has not been initialized yet. Perform an initial scan before attempting to intSet."
                    return state
                Just ptstate -> do
                    let stopsig = (oSendStopSig . sOptions $ state)
                    let chunk_size = (oChunkSize . sOptions $ state)
                    let options = PTScan.ScanOptions chunk_size stopsig

                    ptstate' <- PTWriter.applyWriterS options writer ptstate
                    let newmap = Map.adjust (\_ -> ptstate') stateName (sStates state) :: PTMap
                    let new_state = state{sStates = newmap}
                    return new_state

intSetHelp :: String
intSetHelp = "\nIntSet: Sets the value of an integer to the address of the candidates.\n\nUsage:\n\tintSet [value]\n\n"

intSetCommand =
    ACommand
        { cName = "IntSet"
        , action = intSetAction
        , help = intSetHelp
        }
