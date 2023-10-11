module Main where

import qualified Maps as M
import PeepingTom
import VMIO

main :: IO ()
main = do
    let pid = 1012533 :: Int
    maps <- M.getMap pid
    let maps' = filter M.rwFilter maps
    let maps'' = filter M.notMapping maps'
    let fmaps = maps''
    putStrLn $ "Total maps: " ++ show (length fmaps)
    raw_data <- VMIO.loadMap 10000 fmaps
    -- putStrLn $ show raw_data
    let section1 = raw_data !! 0
    let candidates = extracti32 section1
    let fcandidates = filter (filterEq 0) candidates
    putStrLn $ showCandidates fcandidates
    return ()
