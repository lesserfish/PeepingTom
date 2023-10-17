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
import Text.Printf (printf)

foreign import capi safe "PeepingTom-test.h create_process"
    c_create_process :: IO CInt
foreign import capi safe "PeepingTom-test.h kill_process"
    c_kill :: CInt -> IO ()

create_process :: IO Int
create_process = do
    cint <- c_create_process
    return $ fromIntegral cint

kill :: Int -> IO ()
kill pid = c_kill (fromIntegral pid)

withProcess :: (Int -> IO a) -> IO a
withProcess action = do
    putStrLn $ "Launching process..."
    pid <- create_process
    putStrLn $ printf "Started a process with PID %d" pid
    putStrLn $ printf "Launching test!"
    output <- action pid
    kill pid
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
                let peeptom_matches = length . State.pCandidates $ state
                return $ True
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
                state <- State.scanMap2 [(Type.Type Type.Int64)] fltr maps
                putStrLn $ printf "Second test:\n"
                _ <- State.applyWriter (Writer.writeInt 3) state
                return True
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
                state <- State.scanMap2 [(Type.Type Type.Int64)] fltr maps
                updated_state <- State.updateState 4096 state
                let first_elem_value = State.cData ((State.pCandidates updated_state) !! 0)
                let cast = Conversions.i64FromBS first_elem_value
                return True
            )
    return status

testall :: IO ()
testall = do
    _ <- test1
    _ <- test2
    _ <- test3
    return ()

main :: IO ()
main = testall

-- module Main where

-- import PeepingTom.State
-- import System.IO

-- main :: IO ()
-- main = do
--    PeepingTom.State.debug
--    return ()
