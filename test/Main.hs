{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C
import qualified PeepingTom.Conversions as Conversions
import qualified PeepingTom.Experimental.Fast.Filters as FastFilters
import qualified PeepingTom.Experimental.Fast.State as FastState
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

foreign import capi safe "PeepingTom-test.h get_matchesi"
    c_get_matchesi :: CInt -> CInt -> IO CULong

foreign import capi safe "PeepingTom-test.h get_matches64"
    c_get_matches64 :: CInt -> CInt -> IO CULong

foreign import capi safe "PeepingTom-test.h update_values"
    c_update_values :: CInt -> CInt -> CInt -> CInt -> IO ()

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

get_matches64 :: Int -> Int -> IO Int
get_matches64 pid value = do
    cvalue <- c_get_matches64 (fromIntegral pid) (fromIntegral value)
    return $ fromIntegral cvalue

get_matchesi :: Int -> Int -> IO Int
get_matchesi pid value = do
    cvalue <- c_get_matchesi (fromIntegral pid) (fromIntegral value)
    return $ fromIntegral cvalue

update_values :: Int -> Int -> Int -> Int -> IO ()
update_values pid oldvalue newvalue section = c_update_values (fromIntegral pid) (fromIntegral oldvalue) (fromIntegral newvalue) (fromIntegral section)

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

-- 1. Generate child process
-- 2. Search for Int64 with value 49 with PeepingTom
-- 3. Search for Int64 with value 49 with ScanMem
-- 4. Make sure we get the same amount of matches
test1 :: IO Bool
test1 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = Filters.eqIntX (False, False, False, True) 49
                state <- State.scanMap fltr maps
                pause_process pid
                let peeptom_matches = length . State.psCandidates $ state
                scanmem_matches <- get_matches64 pid 49
                putStrLn $ printf "Test:\n"
                putStrLn $ printf "%d should be equal to %d" peeptom_matches scanmem_matches
                putStrLn $ printf "%s" (if peeptom_matches == scanmem_matches then "Success!\n\n" else "Failure :c\n\n")
                return $ peeptom_matches == scanmem_matches
            )
    return status

-- 1. Generate child process
-- 2. Search for Int64 with value 49 with PeepingTom
-- 3. Write 3 to all addresses
-- 3. Search for Int64 with value 49 with ScanMem
-- 4. Make sure we get 0 matches
test2 :: IO Bool
test2 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = Filters.eqIntX (False, False, False, True) 49
                state <- State.scanMap fltr maps
                putStrLn $ printf "Test:\n"
                _ <- State.applyWriter (Writer.writeInt 3) state
                scanmem_matches <- get_matches64 pid 49
                putStrLn $ printf "%d should be 0" scanmem_matches
                putStrLn $ printf "%s" (if 0 == scanmem_matches then "Success!\n\n" else "Failure :c\n\n")
                return (0 == scanmem_matches)
            )
    return status

-- 1. Generate child process
-- 2. Search for Int64 with value 49 with PeepingTom
-- 3. Use ScanMem to set all Int64 with values equal to 49 to 9
-- 3. Update the results with PeepingTom
-- 4. Check the first element to see if it's bytestring is actually 0

test3 :: IO Bool
test3 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = Filters.eqIntX (False, False, False, True) 49
                state <- State.scanMap fltr maps
                pause_process pid
                update_values pid 49 0 (-1)
                updated_state <- State.updateState state
                let first_elem_value = State.cData ((State.psCandidates updated_state) !! 0)
                let cast = Conversions.i64FromBS first_elem_value
                putStrLn $ printf "Test:\n"
                putStrLn $ printf "%d should be 0" cast
                putStrLn $ printf "%s" (if cast == 0 then "Success!" else "Failure :c")
                return $ cast == 0
            )
    return status

-- 1. Generate child process
-- 2. Search for Int with value 49 with PeepingTom
-- 3. Search for Int with value 49 with ScanMem
-- 4. Make sure we get the same amount of matches
test4 :: IO Bool
test4 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = Filters.eqInt 49
                state <- State.scanMap fltr maps
                pause_process pid
                let peeptom_matches = length . State.psCandidates $ state
                scanmem_matches <- get_matchesi pid 49
                putStrLn $ printf "test:\n"
                putStrLn $ printf "%d should be equal to %d" peeptom_matches scanmem_matches
                putStrLn $ printf "%s" (if peeptom_matches == scanmem_matches then "Success!\n\n" else "Failure :c\n\n")
                return $ peeptom_matches == scanmem_matches
            )
    return status

-- 1. Generate child process
-- 2. Search for Int with value 49 with PeepingTom.Fast
-- 3. Search for Int with value 49 with ScanMem
-- 4. Make sure we get the same amount of matches
test5 :: IO Bool
test5 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = FastFilters.eqInt 49
                state <- FastState.scanMap fltr maps
                pause_process pid
                let peeptom_matches = length . State.psCandidates $ state
                scanmem_matches <- get_matchesi pid 49
                putStrLn $ printf "test:\n"
                putStrLn $ printf "%d should be equal to %d" peeptom_matches scanmem_matches
                putStrLn $ printf "%s" (if peeptom_matches == scanmem_matches then "Success!\n\n" else "Failure :c\n\n")
                return $ peeptom_matches == scanmem_matches
            )
    return status

--
-- 1. Generate child process
-- 2. Search for Int64 with value 49 with PeepingTom.Fast
-- 3. Search for Int64 with value 49 with ScanMem
-- 4. Make sure we get the same amount of matches
test6 :: IO Bool
test6 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = FastFilters.i64Eq 49
                state <- FastState.scanMap fltr maps
                pause_process pid
                let peeptom_matches = length . State.psCandidates $ state
                scanmem_matches <- get_matches64 pid 49
                putStrLn $ printf "Test:\n"
                putStrLn $ printf "%d should be equal to %d" peeptom_matches scanmem_matches
                putStrLn $ printf "%s" (if peeptom_matches == scanmem_matches then "Success!\n\n" else "Failure :c\n\n")
                return $ peeptom_matches == scanmem_matches
            )
    return status

-- 1. Generate child process
-- 2. Search for Int64 with value 49 with PeepingTom
-- 3. Use ScanMem to set the first 20 Int64 with values equal to 49 to 9
-- 3. Update the results with PeepingTom
-- 4. Filter the result using == 9
-- 4. Check that we have 20 results
test7 :: IO Bool
test7 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = Filters.eqIntX (False, False, False, True) 49
                state <- State.scanMap fltr maps
                pause_process pid
                update_values pid 49 0 20
                updated_state <- State.updateState state
                let filtered_state = State.applyFilter (Filters.eqInt 0) updated_state
                let csize = length (State.psCandidates filtered_state)
                putStrLn $ printf "Test:\n"
                putStrLn $ printf "%d should be 20" csize
                putStrLn $ printf "%s" (if csize == 20 then "Success!" else "Failure :c")
                return $ csize == 20
            )
    return status

--
-- 1. Generate child process
-- 2. Search for Int64 with value 49 with PeepingTom
-- 3. Use ScanMem to set the first 20 Int64 with values equal to 49 to 9
-- 3. Update the results with PeepingTom
-- 4. Filter the result using == 9 using Fast.Filters
-- 4. Check that we have 20 results
test8 :: IO Bool
test8 = do
    status <-
        withProcess
            ( \pid -> do
                all_maps <- Maps.getMapInfo pid
                let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
                let fltr = FastFilters.i64Eq 49
                state <- FastState.scanMap fltr maps
                pause_process pid
                update_values pid 49 0 20
                updated_state <- State.updateState state
                filtered_state <- FastFilters.applyFilter (FastFilters.eqInt 0) updated_state
                let csize = length (State.psCandidates filtered_state)
                putStrLn $ printf "Test:\n"
                putStrLn $ printf "%d should be 20" csize
                putStrLn $ printf "%s" (if csize == 20 then "Success!" else "Failure :c")
                return $ csize == 20
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
test 4 = do
    ok <- test4
    if ok then return () else exitWith (ExitFailure 1)
test 5 = do
    ok <- test5
    if ok then return () else exitWith (ExitFailure 1)
test 6 = do
    ok <- test6
    if ok then return () else exitWith (ExitFailure 1)
test 7 = do
    ok <- test7
    if ok then return () else exitWith (ExitFailure 1)
test 8 = do
    ok <- test8
    if ok then return () else exitWith (ExitFailure 1)
test _ = return ()

testall :: IO ()
testall = do
    test 1
    test 2
    test 3
    test 4
    test 5
    test 6
    test 7
    test 8

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
