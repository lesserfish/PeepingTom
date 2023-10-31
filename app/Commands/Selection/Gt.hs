module Commands.Selection.Gt where

import Commands.Base
import Commands.Selection.Helper
import qualified PeepingTom.Fast.Filter as PTFastFilter
import qualified PeepingTom.Fast.Filter as PTFastScan
import qualified PeepingTom.Filter as PTFilter
import State
import Text.Printf
import Text.Read

iFilterFromType :: ScanTypes -> Integer -> Maybe (PTFilter.FilterInfo, PTFastFilter.CFilter)
iFilterFromType Int value = Just (PTFilter.compareInt (> value), PTFastFilter.gtInt value)
iFilterFromType Int8 value = Just (PTFilter.compareIntX (True, False, False, False) (> value), PTFastFilter.i8Gt (fromIntegral value))
iFilterFromType Int16 value = Just (PTFilter.compareIntX (False, True, False, False) (> value), PTFastFilter.i16Gt (fromIntegral value))
iFilterFromType Int32 value = Just (PTFilter.compareIntX (False, False, True, False) (> value), PTFastFilter.i32Gt (fromIntegral value))
iFilterFromType Int64 value = Just (PTFilter.compareIntX (False, False, False, True) (> value), PTFastFilter.i64Gt (fromIntegral value))
iFilterFromType _ _ = Nothing

gtAction :: [String] -> State -> IO State
gtAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: > requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let maybenum = readMaybe numstr
            case maybenum of
                Nothing -> do
                    putStrLn $ printf "Could not understand integer '%s'" numstr
                    return state
                Just num -> do
                    let types = oScanTypes . sOptions $ state
                    let maybefltr = iFilterFromType types num
                    case maybefltr of
                        Nothing -> do
                            putStrLn $ printf "Attempted to scan for integers while scan type is set to %s" (show types)
                            return state
                        Just fltr -> do
                            new_state <- scanAction fltr state
                            return new_state
gtHelp :: String
gtHelp = cmdHelp

gtCommand :: Command
gtCommand =
    ACommand
        { cName = ">"
        , action = gtAction
        , help = gtHelp
        }
