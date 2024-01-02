module Commands.Selection.Eq where

import Commands.Base
import Commands.Selection.Helper
import qualified PeepingTom.Fast.Filter as PTFastFilter
import qualified PeepingTom.Filter as PTFilter
import State
import Text.Printf

iFilterFromType :: ScanTypes -> Integer -> Maybe (PTFilter.FilterInfo, PTFastFilter.CFilter)
iFilterFromType Int value = Just (PTFilter.eqInt value, PTFastFilter.eqInt value)
iFilterFromType Int8 value = Just (PTFilter.i8Eq (fromIntegral value), PTFastFilter.i8Eq (fromIntegral value))
iFilterFromType Int16 value = Just (PTFilter.i16Eq (fromIntegral value), PTFastFilter.i16Eq (fromIntegral value))
iFilterFromType Int32 value = Just (PTFilter.i32Eq (fromIntegral value), PTFastFilter.i32Eq (fromIntegral value))
iFilterFromType Int64 value = Just (PTFilter.i64Eq (fromIntegral value), PTFastFilter.i64Eq (fromIntegral value))
iFilterFromType _ _ = Nothing

eqIntAction :: VArg -> State -> IO State
eqIntAction (VAInt num) state = do
    let types = oScanTypes . sOptions $ state
    let maybefltr = iFilterFromType types num
    case maybefltr of
        Nothing -> do
            putStrLn $ printf "Error: Attempted to scan for Integer while current search type is %s." (show types)
            return state
        Just fltr -> do
            new_state <- scanAction fltr state
            return new_state
eqIntAction _ state = do
    putStrLn $ "Internal error :("
    return state

sFilterFromType :: ScanTypes -> String -> Maybe (PTFilter.FilterInfo, PTFastFilter.CFilter)
sFilterFromType Str str = Just (PTFilter.eqStr str, PTFastFilter.eqStr str)
sFilterFromType _ _ = Nothing

eqStrAction :: VArg -> State -> IO State
eqStrAction (VAStr str) state = do
    let types = oScanTypes . sOptions $ state
    let maybefltr = sFilterFromType types str
    case maybefltr of
        Nothing -> do
            putStrLn $ printf "Error: Attempted to scan for String while current search type is %s." (show types)
            return state
        Just fltr -> do
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
