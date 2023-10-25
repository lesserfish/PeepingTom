module Commands (
    Command (..),
    commands,
) where

import Control.Exception
import Data.Char (toLower)
import Data.List (elem, intersperse)
import qualified Data.Map as Map
import Foreign (new)
import qualified PeepingTom.Filters as PTFilter
import qualified PeepingTom.Maps as PTMap
import qualified PeepingTom.Posix as PTPosix
import qualified PeepingTom.State as PTState
import qualified PeepingTom.Type as PTType
import qualified PeepingTom.Writer as PTWriter
import State
import System.Console.Haskeline (setComplete)
import Text.Printf
import Text.Read

data Command
    = ACommand
        { cName :: String
        , action :: [String] -> State -> IO State
        , help :: String
        }
    | HCommand
        { cName :: String
        , subcommands :: [Command]
        , help :: String
        }

instance Show Command where
    show (ACommand name _ _) = name
    show (HCommand name sub _) = name ++ " > " ++ (show sub)

-- Helper Functions

getStringHelper :: String -> String
getStringHelper ('"' : rest) = reverse (go $ reverse rest)
  where
    go :: String -> String
    go ('"' : rest) = rest
    go str = str
getStringHelper ('\'' : rest) = reverse (go $ reverse rest)
  where
    go :: String -> String
    go ('\'' : rest) = rest
    go str = str
getStringHelper str = str

getString :: [String] -> Maybe String
getString [] = Just $ ""
getString (y : ys)
    | fchr == '"' = Just $ getStringHelper (unwords (y : ys))
    | fchr == '\'' = Just $ getStringHelper (unwords (y : ys))
    | length ys > 0 = Nothing
    | otherwise = Just $ y
  where
    fchr = if length y == 0 then '\0' else y !! 0

-- New

newAction :: [String] -> State -> IO State
newAction args state = do
    let maybename = getString args
    case maybename of
        Nothing -> do
            putStrLn $ printf "Could not understand the argument: %s" (show args)
            return state
        Just name -> do
            if length name == 0
                then do
                    putStrLn $ printf "A name need to be specified"
                    putStrLn $ newHelp
                    return state
                else do
                    let new_state = state{sCurrentState = name}
                    return new_state
newHelp :: String
newHelp = "\nnew: Creates a new empty state with a given name.\n\nUsage:\n\n\tnew [name]\n\n"

newCommand =
    ACommand
        { cName = "new"
        , action = newAction
        , help = newHelp
        }

-- PID

pidAction :: [String] -> State -> IO State
pidAction args state = do
    if length args /= 1
        then do
            putStrLn $ "pid expects a PID value as argument. "
            putStrLn $ pidHelp
            return state
        else do
            let pidstr = args !! 0
            let pid = read pidstr :: Int
            let new_state = state{sPID = pid}
            return new_state
pidHelp :: String
pidHelp = "\npid: Sets the PID of the process on which initial scans are going to execute.\n\nUsage:\n\npid [pid]\n\n"

pidCommand :: Command
pidCommand =
    ACommand
        { cName = "pid"
        , action = pidAction
        , help = pidHelp
        }

filterMap :: RFilter -> PTMap.MapInfo -> PTMap.MapInfo
filterMap RFDefault map = PTMap.filterMap (PTMap.defaultFilter map) map

scanNew :: PTFilter.Filter -> State -> IO State
scanNew fltr state = do
    let types = (oScanTypes . sOptions $ state)
    let stopsig = (oSendStopSig . sOptions $ state)
    let chunk_size = (oChunkSize . sOptions $ state)
    let options = PTState.ScanOptions chunk_size stopsig
    let pid = sPID state
    let stateName = sCurrentState state
    all_maps <- PTMap.getMapInfo pid
    let maps = filterMap (oRFilter . sOptions $ state) all_maps
    ptstate <- PTState.scanMapS options types fltr maps
    putStrLn $ PTState.showState 5 5 ptstate
    let newmap = Map.insert stateName ptstate (sStates state) :: PTMap
    let newstate = state{sStates = newmap}
    return newstate

scanAction' :: PTFilter.Filter -> State -> IO State
scanAction' fltr state = do
    let stateName = sCurrentState (state) :: String
    let maybePTState = Map.lookup stateName (sStates state) :: Maybe PTState.PeepState
    case maybePTState of
        Nothing -> scanNew fltr state
        Just ptState -> do
            let stopsig = (oSendStopSig . sOptions $ state)
            let chunk_size = (oChunkSize . sOptions $ state)
            let options = PTState.ScanOptions chunk_size stopsig

            ptUpdated <- PTState.updateStateS options ptState
            let ptFiltered = PTState.applyFilter fltr ptUpdated
            putStrLn $ PTState.showState 5 5 ptFiltered
            let newmap = Map.adjust (\_ -> ptFiltered) stateName (sStates state) :: PTMap
            let new_state = state{sStates = newmap}
            return new_state

scanAction :: PTFilter.Filter -> State -> IO State
scanAction fltr state = catch (scanAction' fltr state) handler
  where
    handler :: SomeException -> IO State
    handler e = do
        putStrLn $ printf "Exception thrown :(\n\nException: %s\n\n" (show e)
        return state

-- ==

getTypes :: [PTType.Type] -> (Bool, Bool, Bool, Bool)
getTypes types = (i8, i16, i32, i64)
  where
    i8 = elem (PTType.Int8) types
    i16 = elem (PTType.Int16) types
    i32 = elem (PTType.Int32) types
    i64 = elem (PTType.Int64) types
eqAction :: [String] -> State -> IO State
eqAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: == requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let types = getTypes . oScanTypes . sOptions $ state
            let fltr = PTFilter.eqIntX types num
            new_state <- scanAction fltr state
            return new_state

eqHelp :: String
eqHelp = cmdHelp

eqCommand :: Command
eqCommand =
    ACommand
        { cName = "=="
        , action = eqAction
        , help = eqHelp
        }

-- >=
geqAction :: [String] -> State -> IO State
geqAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: >= requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let fltr = PTFilter.compareInt (>= num)
            new_state <- scanAction fltr state
            return new_state

geqHelp :: String
geqHelp = cmdHelp

geqCommand :: Command
geqCommand =
    ACommand
        { cName = ">="
        , action = eqAction
        , help = eqHelp
        }

-- <=
leqAction :: [String] -> State -> IO State
leqAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: <= requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let fltr = PTFilter.compareInt (<= num)
            new_state <- scanAction fltr state
            return new_state

leqHelp :: String
leqHelp = cmdHelp

leqCommand :: Command
leqCommand =
    ACommand
        { cName = "<="
        , action = eqAction
        , help = eqHelp
        }

-- >

gtAction :: [String] -> State -> IO State
gtAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: > requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let fltr = PTFilter.compareInt (> num)
            new_state <- scanAction fltr state
            return new_state

gtHelp :: String
gtHelp = cmdHelp

gtCommand :: Command
gtCommand =
    ACommand
        { cName = ">"
        , action = eqAction
        , help = eqHelp
        }

-- <
ltAction :: [String] -> State -> IO State
ltAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: < requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let fltr = PTFilter.compareInt (< num)
            new_state <- scanAction fltr state
            return new_state

ltHelp :: String
ltHelp = cmdHelp

ltCommand :: Command
ltCommand =
    ACommand
        { cName = "<"
        , action = eqAction
        , help = eqHelp
        }

-- !=
neqAction :: [String] -> State -> IO State
neqAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: != requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let fltr = PTFilter.compareInt (/= num)
            new_state <- scanAction fltr state
            return new_state

neqHelp :: String
neqHelp = cmdHelp

neqCommand :: Command
neqCommand =
    ACommand
        { cName = "!="
        , action = eqAction
        , help = eqHelp
        }

-- $ ()

cmdHelp :: String
cmdHelp = "\n$: Extract candidates from regions of virtual memory.\n\nUsage:\n\n\t$ [Comparison] [Argument]\n\nExamples:\n\n\t$ == 3\n\t$ <= 9\n\t$ > 127\n\n"

cmdCommand :: Command
cmdCommand =
    HCommand
        { cName = "$"
        , subcommands = [eqCommand, geqCommand, leqCommand, gtCommand, ltCommand, neqCommand]
        , help = cmdHelp
        }

-- help

getCommandByStr :: [Command] -> String -> Maybe Command
getCommandByStr commands name
    | length matches == 0 = Nothing
    | otherwise = Just (matches !! 0)
  where
    matches = filter (\c -> (cName c) == name) commands

getHelp :: [String] -> [Command] -> String
getHelp [] [(ACommand _ _ hlp)] = hlp
getHelp [name] cmds = output
  where
    maybecmd = getCommandByStr cmds name
    output = case maybecmd of
        Nothing -> printf "Could not understand '%s'. Type 'help command' to get help" name
        Just (ACommand _ _ hlp) -> hlp
        Just (HCommand _ sc hlp) -> hlp
getHelp (name : rest) cmds = output
  where
    maybecmd = getCommandByStr cmds name
    output = case maybecmd of
        Nothing -> (printf "Could not understand '%s'. Type 'help' to get help" name)
        Just (ACommand _ _ hlp) -> hlp
        Just (HCommand _ sc _) -> getHelp rest sc
getHelp _ _ = "Could not understand input. Type 'help' to get help."
helpHelp :: String
helpHelp =
    "\nPeepingTom: A Virtual memory scanner.\n\nThe following commands are available: \
    \\n\t pid:                         Sets the PID of the process to be scanned by PeepingTom. \
    \\n\t $ [filter] [value]:          Scans the memory or update the candidates, and extracts those that satisfy the filter. \
    \\n\t set [option] [args]:         Sets various options regarding the scan. Type 'help set' to see more.\
    \\n\t list [object]:               Lists several objects. To see a list of available objects type 'help list'.\
    \\n\t update:                      Update the values of the current candidates.\
    \\n\t save [name]:                 Save the current list of candidates.\
    \\n\t load [name]:                 Loads a list of candidates.\
    \\n\t new [name]:                  Creates a new list of candidates.\
    \\n\t reset:                       Resets the current list of candidates.\n\n"

helpAction :: [String] -> State -> IO State
helpAction args state = do
    if length args == 0
        then do
            putStrLn $ helpHelp
            return state
        else do
            putStrLn $ getHelp args commands
            return state

helpCommand :: Command
helpCommand =
    ACommand
        { cName = "help"
        , action = helpAction
        , help = helpHelp
        }

-- list
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
printMemoryHelper (str, state) = printf "%s: \t\t PID: %d %d Candidates in %d Regions" name (PTState.psPID state) (PTState.candidateCount state) (PTState.regionCount state)
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

-- update

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
                    let options = PTState.ScanOptions chunk_size stopsig

                    ptstate' <- PTState.updateStateS options ptstate
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

-- set

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
                    let options = PTState.ScanOptions chunk_size stopsig

                    ptstate' <- PTState.applyWriterS options writer ptstate
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

-- load

loadAction :: [String] -> State -> IO State
loadAction args state = do
    let maybename = getString args
    case maybename of
        Nothing -> do
            putStrLn $ printf "Could not understand the argument: %s" (show args)
            return state
        Just name -> do
            if length name == 0
                then do
                    putStrLn $ printf "A name need to be specified"
                    putStrLn $ loadHelp
                    return state
                else do
                    let maybePTState = Map.lookup name (sStates state) :: Maybe PTState.PeepState
                    case maybePTState of
                        Nothing -> do
                            putStrLn $ printf "There is no state with the name '%s'" name
                            return state
                        Just _ -> do
                            let new_state = state{sCurrentState = name}
                            return new_state

loadHelp :: String
loadHelp = "\nload: Loads a state from memory.\n\nUsage:\n\tload [name]"

loadCommand =
    ACommand
        { cName = "load"
        , action = loadAction
        , help = loadHelp
        }

-- save

saveAction :: [String] -> State -> IO State
saveAction args state = do
    let maybename = getString args
    case maybename of
        Nothing -> do
            putStrLn $ printf "Could not understand the argument: %s" (show args)
            return state
        Just name -> do
            if length name == 0
                then do
                    putStrLn $ printf "A name need to be specified"
                    putStrLn $ saveHelp
                    return state
                else do
                    let maybePTState = Map.lookup (sCurrentState state) (sStates state) :: Maybe PTState.PeepState
                    case maybePTState of
                        Nothing -> do
                            putStrLn $ printf "Current state is not initialized. Not saving."
                            return state
                        Just ptstate -> do
                            let newmap = Map.insert name ptstate (sStates state) :: PTMap
                            let new_state = state{sStates = newmap}
                            return new_state

saveHelp :: String
saveHelp = "\nsave: Saves current state to memory.\n\nUsage:\n\tsave [name]"

saveCommand =
    ACommand
        { cName = "save"
        , action = saveAction
        , help = saveHelp
        }

-- set

setTypeHelper :: String -> Maybe PTType.Type
setTypeHelper "int8" = Just PTType.Int8
setTypeHelper "int16" = Just PTType.Int16
setTypeHelper "int32" = Just PTType.Int32
setTypeHelper "int64" = Just PTType.Int64
setTypeHelper _ = Nothing

setTypeAction :: [String] -> State -> IO State
setTypeAction args state = do
    if length args == 0
        then do
            putStrLn $ "set type requires at least one type as argument"
            putStrLn $ setTypeHelp
            return state
        else do
            types <- loop args
            let new_options = (sOptions state){oScanTypes = types}
            let new_state = state{sOptions = new_options}
            return new_state
  where
    loop :: [String] -> IO [PTType.Type]
    loop [] = return []
    loop (str : rest) = do
        let maybetype = setTypeHelper $ (map toLower str)
        case maybetype of
            Nothing -> do
                putStrLn $ printf "Could not understand type '%s'" str
                return []
            Just t -> do
                tail <- loop rest
                return $ [t] ++ tail

setTypeHelp :: String
setTypeHelp = "\nset type: Sets the scan type.\n\nUsage:\n\tset type [type] [type] [type]\n\nExamples:\n\tset type Int32 Int64"

setTypeCommand =
    ACommand
        { cName = "type"
        , action = setTypeAction
        , help = setTypeHelp
        }

setSigStopHelper :: String -> Maybe Bool
setSigStopHelper "true" = Just True
setSigStopHelper "false" = Just False
setSigStopHelper _ = Nothing

setSigStopAction :: [String] -> State -> IO State
setSigStopAction args state = do
    if length args /= 1
        then do
            putStrLn $ "set sig_stop requires one argument: true or false"
            putStrLn $ setSigStopHelp
            return state
        else do
            let arg = args !! 0
            let maybebool = setSigStopHelper (map toLower arg)
            case maybebool of
                Nothing -> do
                    putStrLn $ printf "Could not understand %s" arg
                    return state
                Just sigstop -> do
                    let new_options = (sOptions state){oSendStopSig = sigstop}
                    let newstate = state{sOptions = new_options}
                    return newstate

setSigStopHelp :: String
setSigStopHelp = "\nset send_stopsig: Sets whether or not PeepingTop stops an application before scanning/writting to its virtual memory.\n\nUsage:\n\tset send_stopsig [Bool]"

setSigStopCommand =
    ACommand
        { cName = "send_stopsig"
        , action = setSigStopAction
        , help = setSigStopHelp
        }

setChunkSizeAction :: [String] -> State -> IO State
setChunkSizeAction args state = do
    if length args /= 1
        then do
            putStrLn $ "set chunk_size requires one integer argument"
            putStrLn $ setChunkSizeHelp
            return state
        else do
            let arg = args !! 0
            let maybeint = readMaybe arg :: Maybe Int
            case maybeint of
                Nothing -> do
                    putStrLn $ printf "Could not understand %s" arg
                    return state
                Just chunk_size -> do
                    let new_options = (sOptions state){oChunkSize = chunk_size}
                    let newstate = state{sOptions = new_options}
                    return newstate

setChunkSizeHelp :: String
setChunkSizeHelp = "\nset chunk_size: Sets the size of chunks being read by PeepingTom.\n\nUsage:\n\tset chunk_size [Integer]"

setChunkSizeCommand =
    ACommand
        { cName = "chunk_size"
        , action = setChunkSizeAction
        , help = setChunkSizeHelp
        }

setHelp :: String
setHelp = "\nset: Sets various scan options.\n\nUsage:\n\tset [option] [args]\nPossible Options: type, chunk_size, send_stopsig."
setCommand =
    HCommand
        { cName = "set"
        , subcommands = [setTypeCommand, setSigStopCommand, setChunkSizeCommand]
        , help = setHelp
        }

-- reset

resetAction :: [String] -> State -> IO State
resetAction args state = do
    if length args > 0
        then do
            putStrLn $ "reset does not require any arguments"
            putStrLn $ resetHelp
            return state
        else do
            let name = sCurrentState state
            let new_map = Map.delete name (sStates state)
            let new_state = state{sStates = new_map}
            return new_state

resetHelp :: String
resetHelp = "\nreset: Resets the current list"

resetCommand =
    ACommand
        { cName = "reset"
        , action = resetAction
        , help = resetHelp
        }

commands = [cmdCommand, newCommand, pidCommand, helpCommand, listCommand, updateCommand, intSetCommand, loadCommand, saveCommand, setCommand, resetCommand]

-- Todo: Implement the rest!
