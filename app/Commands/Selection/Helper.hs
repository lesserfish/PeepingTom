module Commands.Selection.Helper where

import Control.Exception
import qualified Data.Map as Map
import qualified PeepingTom.Fast.Filter as PTFastFilter
import qualified PeepingTom.Fast.Scan as PTFastScan
import qualified PeepingTom.Filter as PTFilter
import qualified PeepingTom.Map as PTMap
import qualified PeepingTom.Scan as PTScan
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
filterMap RFDefault map = PTMap.filterRegions (PTMap.defaultRFilter map) map
filterMap RFReadPerm map = PTMap.filterRegions (PTMap.rFilterR) map
filterMap RFWritePerm map = PTMap.filterRegions (PTMap.rFilterW) map
filterMap RFAll map = map

scanNew :: PTFilter.FilterInfo -> State -> IO State
scanNew fltr state = do
    let types = (oScanTypes . sOptions $ state)
    let stopsig = (oSendStopSig . sOptions $ state)
    let chunk_size = (oChunkSize . sOptions $ state)
    let options = PTScan.ScanOptions chunk_size stopsig
    let pid = sPID state
    let stateName = sCurrentState state
    all_maps <- PTMap.extractRegions pid
    let maps = filterMap (oRFilter . sOptions $ state) all_maps
    ptstate <- PTScan.scanMapS options fltr maps
    putStrLn $ PTState.showState 5 5 ptstate
    let newmap = Map.insert stateName ptstate (sStates state) :: PTMap
    let newstate = state{sStates = newmap}
    return newstate

scanNewFast :: PTFastFilter.CFilter -> State -> IO State
scanNewFast fltr state = do
    let types = (oScanTypes . sOptions $ state)
    let stopsig = (oSendStopSig . sOptions $ state)
    let chunk_size = (oChunkSize . sOptions $ state)
    let options = PTScan.ScanOptions chunk_size stopsig
    let pid = sPID state
    let stateName = sCurrentState state
    all_maps <- PTMap.extractRegions pid
    let maps = filterMap (oRFilter . sOptions $ state) all_maps
    ptstate <- PTFastScan.scanMapS options fltr maps
    putStrLn $ PTState.showState 5 5 ptstate
    let newmap = Map.insert stateName ptstate (sStates state) :: PTMap
    let newstate = state{sStates = newmap}
    return newstate

updateScan :: PTFilter.FilterInfo -> PTState.PeepState -> State -> IO State
updateScan fltr ptState state = do
    let stateName = sCurrentState (state) :: String
    let stopsig = (oSendStopSig . sOptions $ state)
    let chunk_size = (oChunkSize . sOptions $ state)
    let options = PTScan.ScanOptions chunk_size stopsig

    ptUpdated <- PTScan.updateStateS options ptState
    let ptFiltered = PTFilter.applyFilter fltr ptUpdated
    putStrLn $ PTState.showState 5 5 ptFiltered
    let newmap = Map.adjust (\_ -> ptFiltered) stateName (sStates state) :: PTMap
    let new_state = state{sStates = newmap}
    return new_state

scanAction' :: (PTFilter.FilterInfo, PTFastFilter.CFilter) -> State -> IO State
scanAction' (fltr, cfltr) state = do
    let fastmode = oFastMode . sOptions $ state
    if fastmode
        then do
            let stateName = sCurrentState (state) :: String
            let maybePTState = Map.lookup stateName (sStates state) :: Maybe PTState.PeepState
            case maybePTState of
                Nothing -> scanNewFast cfltr state
                Just ptState -> updateScan fltr ptState state -- So far, there is no updateScanFast
        else do
            let stateName = sCurrentState (state) :: String
            let maybePTState = Map.lookup stateName (sStates state) :: Maybe PTState.PeepState
            case maybePTState of
                Nothing -> scanNew fltr state
                Just ptState -> updateScan fltr ptState state

scanAction :: (PTFilter.FilterInfo, PTFastFilter.CFilter) -> State -> IO State
scanAction fltr state = catch (scanAction' fltr state) handler
  where
    handler :: SomeException -> IO State
    handler e = do
        putStrLn $ printf "Exception thrown :(\n\nException: %s\n\n" (show e)
        return state

cmdHelp :: String
cmdHelp = "\n$: Extract candidates from regions of virtual memory.\n\nUsage:\n\n\t$ [Comparison] [Argument]\n\nExamples:\n\n\t$ == 3\n\t$ <= 9\n\t$ > 127\n\n"
