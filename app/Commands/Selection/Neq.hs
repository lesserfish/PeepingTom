module Commands.Selection.Neq where

import Commands.Base
import Commands.Selection.Helper
import qualified PeepingTom.Fast.Filter as PTFastFilter
import qualified PeepingTom.Filter as PTFilter
import State
import Text.Printf
import Text.Read

iFilterFromType :: ScanTypes -> Integer -> Maybe (PTFilter.FilterInfo, PTFastFilter.CFilter)
iFilterFromType Int value = Just (PTFilter.compareInt (/= value), PTFastFilter.neqInt value)
iFilterFromType Int8 value = Just (PTFilter.compareIntX (True, False, False, False) (/= value), PTFastFilter.i8Neq (fromIntegral value))
iFilterFromType Int16 value = Just (PTFilter.compareIntX (False, True, False, False) (/= value), PTFastFilter.i16Neq (fromIntegral value))
iFilterFromType Int32 value = Just (PTFilter.compareIntX (False, False, True, False) (/= value), PTFastFilter.i32Neq (fromIntegral value))
iFilterFromType Int64 value = Just (PTFilter.compareIntX (False, False, False, True) (/= value), PTFastFilter.i64Neq (fromIntegral value))
iFilterFromType _ _ = Nothing

neqAction :: [String] -> State -> IO State
neqAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: != requires one and only one additional argument: The integer value to be used in the comparison!"
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
neqHelp :: String
neqHelp = cmdHelp

neqCommand :: Command
neqCommand =
    ACommand
        { cName = "!="
        , action = neqAction
        , help = neqHelp
        }
