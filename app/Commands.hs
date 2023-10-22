module Commands (
    Command (..),
    commands,
) where

import Control.Exception
import qualified Data.Map as Map
import Foreign (new)
import qualified PeepingTom.Filters as PTFilter
import qualified PeepingTom.Maps as PTMap
import qualified PeepingTom.Posix as PTPosix
import qualified PeepingTom.State as PTState
import qualified PeepingTom.Type as PTType
import qualified PeepingTom.Writer as PTWriter
import State
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
newHelp = "\n\nnew: Creates a new empty state with a given name.\n\nUsage:\n\n\tnew [name]\n\n"

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
            putStrLn $ pidHelp
            return state
        else do
            let pidstr = args !! 0
            let pid = read pidstr :: Int
            let new_state = state{sPID = pid}
            return new_state
pidHelp :: String
pidHelp = "\n\npid: Sets the PID of the process on which initial scans are going to execute.\n\nUsage:\n\npid [pid]\n\n"

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
    let pid = sPID state
    let stateName = sCurrentState state
    all_maps <- PTMap.getMapInfo pid
    let maps = filterMap (oRFilter . sOptions $ state) all_maps
    ptstate <- PTState.scanMapS (oChunkSize . sOptions $ state) (types) fltr maps
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
            let chunk_size = oChunkSize . sOptions $ state
            ptUpdated <- PTState.updateState chunk_size ptState
            let ptFiltered = PTState.applyFilter fltr ptUpdated
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
            let fltr = PTFilter.eqInt num
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
cmdHelp = "$: Extract candidates from regions of virtual memory.\n\nUsage:\n\n\t$ [Comparison] [Argument]\n\nExamples:\n\n\t$ == 3\n\t$ <= 9\n\t$ > 127\n\n"

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
    "\n\nPeepingTom: A Virtual memory scanner.\n\nThe following commands are available: \
    \\n\t pid:                         Sets the PID of the process to be scanned by PeepingTom. \
    \\n\t $ [filter] [value]:          Scans the memory or update the candidates, and extracts those that satisfy the filter. \
    \\n\t set [option] [args]:         Sets various options regarding the scan. Type 'help set' to see more.\
    \\n\t list [object]:               Lists several objects. To see a list of available objects type 'help list'.\
    \\n\t delete [object]:             Deletes an object. To see a list of objects that can be deleted type 'help delete'.\
    \\n\t filter map [filter]:         Filters the list of virtual memory regions. To see a list of available filters type 'list rfilter'.\
    \\n\t filter candidate [filter]:   Filters the list of candidates. To see a list of available filters type 'list filter'.\
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
listStateHelp = "list state: Shows the current state.\n\nUsage:\n\tlist state [number of regions] [number of candidates]"

listStateCommand =
    ACommand
        { cName = "state"
        , action = listStateAction
        , help = listStateHelp
        }

listHelp :: String
listHelp = "list: Prints an object to stdout.\n\nUsage:\n\tlist [object]\n\nPossible objects are: state, filter, rfilter, memory"

listCommand :: Command
listCommand =
    HCommand
        { cName = "list"
        , subcommands = [listStateCommand]
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
                    ptstate' <- PTState.updateState (oChunkSize . sOptions $ state) ptstate
                    let newmap = Map.adjust (\_ -> ptstate') stateName (sStates state) :: PTMap
                    let new_state = state{sStates = newmap}
                    return new_state

updateHelp :: String
updateHelp = "update: Updates the values of the current candidates.\n\nUsage:\n\tupdate"

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
            putStrLn $ "intSet required one integer input"
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
                    ptstate' <- PTState.applyWriter writer ptstate
                    let newmap = Map.adjust (\_ -> ptstate') stateName (sStates state) :: PTMap
                    let new_state = state{sStates = newmap}
                    return new_state

intSetHelp :: String
intSetHelp = "IntSet: Sets the value of an integer to the address of the candidates.\n\nUsage:\n\tintSet [value]\n\n"

intSetCommand =
    ACommand
        { cName = "IntSet"
        , action = intSetAction
        , help = intSetHelp
        }

commands = [cmdCommand, newCommand, pidCommand, helpCommand, listCommand, updateCommand, intSetCommand]

-- Todo: Implement the rest!
