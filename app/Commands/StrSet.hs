module Commands.StrSet (strSetCommand) where

import Commands.Base
import Commands.Selection.Helper
import qualified Data.Map as Map
import qualified PeepingTom.State as PTState
import qualified PeepingTom.Writer as PTWriter
import State

strSetAction :: [String] -> State -> IO State
strSetAction args state = do
    let va = parseVA $ unwords args
    case va of
        VAStr str -> do
            let writer = PTWriter.writeStr_ str
            let stateName = sCurrentState state
            let mptstate = Map.lookup stateName (sStates state)
            case mptstate of
                Nothing -> do
                    putStrLn $ "State has not been initialized yet. Perform an initial scan before attempting to StrSet."
                    return state
                Just ptstate -> do
                    let stopsig = (oSendStopSig . sOptions $ state)
                    let chunk_size = (oChunkSize . sOptions $ state)
                    let options = PTState.ScanOptions chunk_size stopsig
                    ptstate' <- PTState.applyWriterS options writer ptstate
                    let newmap = Map.adjust (\_ -> ptstate') stateName (sStates state) :: PTMap
                    let new_state = state{sStates = newmap}
                    return new_state

strSetHelp :: String
strSetHelp = "\nstrSet: Sets the address of the candidates to a given string.\n\nUsage:\n\tStrSet \"String\"\n\n"

strSetCommand =
    ACommand
        { cName = "StrSet"
        , action = strSetAction
        , help = strSetHelp
        }
