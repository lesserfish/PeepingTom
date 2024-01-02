module Commands.Set (setCommand) where

import Commands.Base
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified PeepingTom.State as PTState
import qualified PeepingTom.Type as PTType
import State
import Text.Printf (printf)
import Text.Read

setTypeHelper :: String -> Maybe ScanTypes
setTypeHelper "int8" = Just Int8
setTypeHelper "int16" = Just Int16
setTypeHelper "int32" = Just Int32
setTypeHelper "int64" = Just Int64
setTypeHelper "int" = Just Int
setTypeHelper "string" = Just Str
setTypeHelper _ = Nothing

setTypeAction :: [String] -> State -> IO State
setTypeAction args state = do
    if length args /= 1
        then do
            putStrLn $ "set type requires one argument: The type to scan for"
            putStrLn $ setTypeHelp
            return state
        else do
            let arg = args !! 0
            let maybetype = setTypeHelper arg
            case maybetype of
                Nothing -> do
                    putStrLn $ printf "Could not understand type '%s'." arg
                    putStrLn $ setTypeHelp
                    return state
                Just types -> do
                    let new_options = (sOptions state){oScanTypes = types}
                    let new_state = state{sOptions = new_options}
                    return new_state

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

setRFilterHelper :: String -> Maybe RFilter
setRFilterHelper "default" = Just RFDefault
setRFilterHelper "r" = Just RFReadPerm
setRFilterHelper "w" = Just RFWritePerm
setRFilterHelper "all" = Just RFAll
setRFilterHelper _ = Nothing

setRFilterAction :: [String] -> State -> IO State
setRFilterAction args state = do
    if length args /= 1
        then do
            putStrLn $ "set type requires an argument: The filter to be used in the regions"
            putStrLn $ setRFilterHelp
            return state
        else do
            let maybefilt = setRFilterHelper (args !! 0)
            case maybefilt of
                Nothing -> do
                    putStrLn $ printf "Could not understand %s" (args !! 0)
                    return state
                Just rfilt -> do
                    let new_options = (sOptions state){oRFilter = rfilt}
                    let new_state = state{sOptions = new_options}
                    return new_state

setRFilterHelp :: String
setRFilterHelp = "\nset rfilter: Sets the rule that will decide which regions of virtual memory to scan.\n\nUsage:\n\tset rfilter [RFilter].\n\nValid options of RFilter are:\n\tdefault: All regions with read/write permission that are not mappings of files, with the exception of the main executable.\n\tr: All regions with read permission\n\tw: All regions with write permission\n\tall: All regions"

setRFilterCommand =
    ACommand
        { cName = "rfilter"
        , action = setRFilterAction
        , help = setRFilterHelp
        }

setHelp :: String
setHelp = "\nset: Sets various scan options.\n\nUsage:\n\tset [option] [args]\nPossible Options: type, chunk_size, send_stopsig."
setCommand =
    HCommand
        { cName = "set"
        , subcommands = [setTypeCommand, setSigStopCommand, setChunkSizeCommand, setRFilterCommand]
        , help = setHelp
        }
