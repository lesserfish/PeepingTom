{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C
import qualified PeepingTom.Conversions as Conversions
import qualified PeepingTom.Filters as Filters
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.State as State
import qualified PeepingTom.Type as Type
import qualified PeepingTom.Writer as Writer
import System.Environment
import System.Exit
import Text.Printf (printf)

foreign import capi safe "PeepingTom-test.h create_process"
    c_create_process :: IO CInt

foreign import capi safe "PeepingTom-test.h get_matches"
    c_get_matches :: CInt -> CInt -> IO CULong

foreign import capi safe "PeepingTom-test.h update_values"
    c_update_values :: CInt -> CInt -> CInt -> IO ()

foreign import capi safe "PeepingTom-test.h terminate_process"
    c_terminate_process :: CInt -> IO ()

foreign import capi safe "PeepingTom-test.h init_scanmem"
    c_init_scanmem :: IO ()

foreign import capi safe "PeepingTom-test.h pause_process"
    c_pause_process :: CInt -> IO ()

pause_process :: Int -> IO ()
pause_process pid = c_pause_process (fromIntegral pid)

create_process :: IO Int
create_process = do
    cint <- c_create_process
    return $ fromIntegral cint

get_matches :: Int -> Int -> IO Int
get_matches pid value = do
    cvalue <- c_get_matches (fromIntegral pid) (fromIntegral value)
    return $ fromIntegral cvalue

update_values :: Int -> Int -> Int -> IO ()
update_values pid oldvalue newvalue = c_update_values (fromIntegral pid) (fromIntegral oldvalue) (fromIntegral newvalue)

terminate_process :: Int -> IO ()
terminate_process pid = do
    c_terminate_process (fromIntegral pid)

withProcess :: (Int -> IO a) -> IO a
withProcess action = do
    putStrLn $ "Launching process..."
    pid <- create_process
    pause_process pid
    putStrLn $ printf "Started a process with PID %d" pid
    putStrLn $ printf "Launching test!"
    output <- action pid
    terminate_process pid
    return output

test1 :: IO Bool
test1 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = Filters.eqInteger 49
                state <- State.scanMap [(Type.Type Type.Int64)] fltr maps
                pause_process pid
                let peeptom_matches = length . State.psCandidates $ state
                scanmem_matches <- get_matches pid 49
                putStrLn $ printf "First test:\n"
                putStrLn $ printf "%d should be equal to %d" peeptom_matches scanmem_matches
                putStrLn $ printf "%s" (if peeptom_matches == scanmem_matches then "Success!\n\n" else "Failure :c\n\n")
                return $ peeptom_matches == scanmem_matches
            )
    return status

test2 :: IO Bool
test2 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = Filters.eqInteger 49
                state <- State.scanMap [(Type.Type Type.Int64)] fltr maps
                putStrLn $ printf "Second test:\n"
                _ <- State.applyWriter (Writer.writeInt 3) state
                scanmem_matches <- get_matches pid 49
                putStrLn $ printf "%d should be 0" scanmem_matches
                putStrLn $ printf "%s" (if 0 == scanmem_matches then "Success!\n\n" else "Failure :c\n\n")
                return (0 == scanmem_matches)
            )
    return status

test3 :: IO Bool
test3 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = Filters.eqInteger 49
                state <- State.scanMap [(Type.Type Type.Int64)] fltr maps
                pause_process pid
                update_values pid 49 0
                updated_state <- State.updateState 4096 state
                let first_elem_value = State.cData ((State.psCandidates updated_state) !! 0)
                let cast = Conversions.i64FromBS first_elem_value
                putStrLn $ printf "Third test:\n"
                putStrLn $ printf "%d should be 0" cast
                putStrLn $ printf "%s" (if cast == 0 then "Success!" else "Failure :c")
                return $ cast == 0
            )
    return status

test :: Int -> IO ()
test 1 = do
    ok <- test1
    if ok then return () else exitWith (ExitFailure 1)
test 2 = do
    ok <- test2
    if ok then return () else exitWith (ExitFailure 1)
test 3 = do
    ok <- test3
    if ok then return () else exitWith (ExitFailure 1)
test _ = return ()

testall :: IO ()
testall = do
    test 1
    test 2
    test 3

main :: IO ()
main = do
    _ <- c_init_scanmem
    args <- getArgs
    case args of
        [] -> testall
        (arg : _) -> do
            let maybeInt = safeRead arg :: Maybe Int
            case maybeInt of
                Just intVal -> do
                    _ <- test intVal
                    return ()
                Nothing -> putStrLn $ "Invalid input: " ++ arg
    return ()

safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
