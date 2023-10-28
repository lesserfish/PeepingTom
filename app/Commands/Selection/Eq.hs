module Commands.Selection.Eq where

import Commands.Base
import Commands.Selection.Helper
import qualified PeepingTom.Filters as PTFilter
import State
import Text.Printf

eqIntAction :: VArg -> State -> IO State
eqIntAction (VAInt num) state = do
    let types = getTypes . oScanTypes . sOptions $ state
    let fltr = PTFilter.eqIntX types num
    new_state <- scanAction fltr state
    return new_state
eqIntAction _ state = do
    putStrLn $ "Internal error :("
    return state

eqStrAction :: VArg -> State -> IO State
eqStrAction (VAStr str) state = do
    let fltr = PTFilter.eqStr str
    new_state <- scanAction fltr state
    return new_state
eqStrAction _ state = do
    putStrLn $ "Internal error :("
    return state

eqAction :: [String] -> State -> IO State
eqAction strs state = do
    case (parseVA (unwords strs)) of
        VAInt i -> eqIntAction (VAInt i) state
        VAStr s -> eqStrAction (VAStr s) state
        VADbl d -> do
            putStrLn $ printf "Scanning for rational types is still not supported! Sorry!"
            return state
        _ -> do
            putStrLn $ printf "Could not understand '%s'" (unwords strs)
            return state

eqHelp :: String
eqHelp = cmdHelp

eqCommand :: Command
eqCommand =
    ACommand
        { cName = "=="
        , action = eqAction
        , help = eqHelp
        }
