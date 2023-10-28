{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C
import qualified PeepingTom.Conversions as Conversions
import qualified PeepingTom.Experimental.Fast.State as Fast
import qualified PeepingTom.Filters as Filters
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.State as State
import qualified PeepingTom.Type as Type
import qualified PeepingTom.Writer as Writer
import System.CPUTime
import System.Environment
import System.Exit
import Text.Printf (printf)

foreign import capi safe "PeepingTom-speedtest.h create_process"
    c_create_process :: IO CInt

foreign import capi safe "PeepingTom-speedtest.h run_scanmem"
    c_run_scanmem :: CInt -> CInt -> IO ()

foreign import capi safe "PeepingTom-speedtest.h terminate_process"
    c_terminate_process :: CInt -> IO ()

create_process :: IO Int
create_process = do
    cint <- c_create_process
    return $ fromIntegral cint

run_scanmem :: Int -> Int -> IO ()
run_scanmem pid value = do
    c_run_scanmem (fromIntegral pid) (fromIntegral value)

terminate_process :: Int -> IO ()
terminate_process pid = do
    c_terminate_process (fromIntegral pid)

withProcess :: (Int -> IO a) -> IO a
withProcess action = do
    putStrLn $ "Launching process..."
    pid <- create_process
    putStrLn $ printf "Started a process with PID %d" pid
    output <- action pid
    terminate_process pid
    return output

time :: (IO ()) -> IO Double
time action = do
    start <- getCPUTime
    action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10 ^ 6)
    return diff

run_peeptom :: Int -> Integer -> IO ()
run_peeptom pid value = do
    all_maps <- Maps.getMapInfo pid
    let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
    let fltr = Filters.eqInt value
    state <- State.scanMap fltr maps
    return ()

run_peeptom_fast :: Int -> Integer -> IO ()
run_peeptom_fast pid value = do
    all_maps <- Maps.getMapInfo pid
    let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
    let fltr = Filters.eqInt value
    state <- Fast.scanMap (Fast.CFilter [Type.Int32]) maps
    return ()

main :: IO ()
main = do
    withProcess
        ( \pid -> do
            scanmem <- time $ run_scanmem pid 42
            peeptom <- time $ run_peeptom pid 42
            peeptom_fast <- time $ run_peeptom_fast pid 42
            putStrLn $ printf "\n\nScanMem: %f" scanmem
            putStrLn $ printf "PeepingTom: %f" peeptom
            putStrLn $ printf "PeepingTom Fast: %f" peeptom_fast
            putStrLn $ printf "\nDifference: %f" (peeptom / scanmem)
            putStrLn $ printf "Difference Fast: %f" (peeptom_fast / scanmem)
        )
