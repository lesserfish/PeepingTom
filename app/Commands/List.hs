module Commands.List (listCommand) where

import Commands.Base
import qualified Data.Map as Map
import qualified PeepingTom.State as PTState
import State
import Text.Printf (printf)
import Text.Read

parseInts :: [String] -> Maybe [Int]
parseInts [] = Just $ []
parseInts (y : str) = case (readMaybe y) of
    Nothing -> Nothing
    Just xy -> case parseInts str of
        Nothing -> Nothing
        Just xs -> Just $ xy : xs

listStateAction :: [String] -> State -> IO State
listStateAction args state = do
    let maybenargs = parseInts args
    case maybenargs of
        Nothing -> do
            putStrLn $ "Failed to parse arguments to list state :("
            return state
        Just nargs -> do
            let nreg = if length nargs >= 1 then nargs !! 0 else 5
            let ncan = if length nargs >= 2 then nargs !! 1 else 5
            let mptstate = Map.lookup (sCurrentState state) (sStates state)
            case mptstate of
                Nothing -> do
                    putStrLn $ "State has not been initialized yet. Perform an initial scan before attempting to list it."
                    return state
                Just ptstate -> do
                    putStrLn $ PTState.showState nreg ncan ptstate
                    return state

listStateHelp :: String
listStateHelp = "\nlist state: Shows the current state.\n\nUsage:\n\tlist state [number of regions] [number of candidates]"

listStateCommand =
    ACommand
        { cName = "state"
        , action = listStateAction
        , help = listStateHelp
        }

printMemoryHelper :: (String, PTState.PeepState) -> String
printMemoryHelper (str, state) = printf "%s: \t\t PID: %d %d Candidates in %d Regions" name (PTState.psPID state) (PTState.matchCount state) (PTState.regionCount state)
  where
    name = if length str <= 10 then str else (take 10 str) ++ "..."

showLst :: [String] -> String
showLst [] = ""
showLst (y : ys) = y ++ "\n" ++ showLst ys

printMemory :: PTMap -> String
printMemory map = showLst strings
  where
    strings = Map.foldrWithKey (\k v acc -> printMemoryHelper (k, v) : acc) [] map

listMemoryAction :: [String] -> State -> IO State
listMemoryAction args state = do
    if length args > 0
        then do
            putStrLn $ "list memory does not require any arguments."
            return state
        else do
            putStrLn $ printMemory (sStates state)
            return state

listMemoryHelp :: String
listMemoryHelp = "\nlist memory: Prints the memory"

listMemoryCommand =
    ACommand
        { cName = "memory"
        , action = listMemoryAction
        , help = listMemoryHelp
        }

listOptionAction :: [String] -> State -> IO State
listOptionAction args state = do
    if length args > 0
        then do
            putStrLn $ "list options does not require any arguments."
            return state
        else do
            putStrLn $ printf "PID: %d" (sPID state)
            putStrLn $ show (sOptions state)
            return state

listOptionHelp :: String
listOptionHelp = "\nlist options: Prints the current options"

listOptionCommand =
    ACommand
        { cName = "options"
        , action = listOptionAction
        , help = listOptionHelp
        }

listHelp :: String
listHelp = "\nlist: Prints an object to stdout.\n\nUsage:\n\tlist [object]\n\nPossible objects are: state, filter, rfilter, memory"

listCommand :: Command
listCommand =
    HCommand
        { cName = "list"
        , subcommands = [listStateCommand, listMemoryCommand, listOptionCommand]
        , help = listHelp
        }
