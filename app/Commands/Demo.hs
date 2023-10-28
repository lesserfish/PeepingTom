module Commands.Demo where

import Commands.Base
import Control.Exception
import Data.List (elem)
import qualified Data.Map as Map
import qualified PeepingTom.Experimental.Fast.State as Fast
import qualified PeepingTom.Filters as PTFilter
import qualified PeepingTom.Maps as PTMap
import qualified PeepingTom.State as PTState
import qualified PeepingTom.Type as PTType
import State
import Text.Printf (printf)
import Text.Read

data VArg
    = VAInt Integer
    | VADbl Double
    | VAStr String
    | VAInvalid
    deriving (Show)

isInt :: String -> Bool
isInt str = case mayi of
    Nothing -> False
    Just i -> True
  where
    mayi = readMaybe str :: Maybe Int

isDbl :: String -> Bool
isDbl str = case mayd of
    Nothing -> False
    Just d -> True
  where
    mayd = readMaybe str :: Maybe Double

isStr :: String -> Bool
isStr [] = False
isStr [c] = False
isStr str = check
  where
    check = c1 == '"' && cf == '"'
    c1 = head str
    cf = last str

removeQuotes :: String -> String
removeQuotes str = init . tail $ str

parseVA :: String -> VArg
parseVA input
    | isInt input = VAInt (read input)
    | isDbl input = VADbl (read input)
    | isStr input = VAStr (removeQuotes input)
    | otherwise = VAInvalid

filterMap :: RFilter -> PTMap.MapInfo -> PTMap.MapInfo
filterMap RFDefault map = PTMap.filterMap (PTMap.defaultFilter map) map
filterMap RFReadPerm map = PTMap.filterMap (PTMap.filterR) map
filterMap RFWritePerm map = PTMap.filterMap (PTMap.filterW) map
filterMap RFAll map = map

scanNew :: Fast.CFilter -> State -> IO State
scanNew fltr state = do
    let types = (oScanTypes . sOptions $ state)
    let stopsig = (oSendStopSig . sOptions $ state)
    let chunk_size = (oChunkSize . sOptions $ state)
    let options = PTState.ScanOptions chunk_size stopsig
    let pid = sPID state
    let stateName = sCurrentState state
    all_maps <- PTMap.getMapInfo pid
    let maps = filterMap (oRFilter . sOptions $ state) all_maps
    ptstate <- Fast.scanMapS options (Fast.CFilter [PTType.Int32]) maps
    putStrLn $ PTState.showState 5 5 ptstate
    let newmap = Map.insert stateName ptstate (sStates state) :: PTMap
    let newstate = state{sStates = newmap}
    return newstate

scanAction' :: Fast.CFilter -> State -> IO State
scanAction' fltr state = do
    let stateName = sCurrentState (state) :: String
    let maybePTState = Map.lookup stateName (sStates state) :: Maybe PTState.PeepState
    case maybePTState of
        Nothing -> scanNew fltr state
        Just ptState -> do
            return state

scanAction :: Fast.CFilter -> State -> IO State
scanAction fltr state = catch (scanAction' fltr state) handler
  where
    handler :: SomeException -> IO State
    handler e = do
        putStrLn $ printf "Exception thrown :(\n\nException: %s\n\n" (show e)
        return state

cmdHelp :: String
cmdHelp = "\n$: Extract candidates from regions of virtual memory.\n\nUsage:\n\n\t$ [Comparison] [Argument]\n\nExamples:\n\n\t$ == 3\n\t$ <= 9\n\t$ > 127\n\n"

eqIntAction :: VArg -> State -> IO State
eqIntAction (VAInt num) state = do
    let types = [PTType.Int32]
    let fltr = Fast.CFilter types
    new_state <- scanAction fltr state
    return new_state
eqIntAction _ state = do
    putStrLn $ "Internal error :("
    return state

eqStrAction :: VArg -> State -> IO State
eqStrAction (VAStr str) state = do
    new_state <- scanAction (Fast.CFilter []) state
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
