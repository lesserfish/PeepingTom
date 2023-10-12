{-# LANGUAGE GADTs #-}

module PeepingTom where

import qualified Data.ByteString as B
import qualified Maps as M
import Type
import VMIO

data Candidate = forall a. (Show a, Type a) => Candidate {caddress :: Int, cvalue :: a, pvalue :: a, cregion :: M.Region}
type Candidates = [Candidate]

instance Show Candidate where
    show (Candidate addr cv pv reg) =
        "Address: "
            ++ show addr
            ++ "\t Value: "
            ++ showT cv
            ++ "\t Previous Value: "
            ++ showT pv
            ++ "\t Region ID: "
            ++ show (M.regionID reg)

showCandidates :: Candidates -> String
showCandidates [] = ""
showCandidates (y : ys) = show y ++ "\n" ++ showCandidates ys

updateValue :: Candidate -> B.ByteString -> Candidate
updateValue (Candidate a v b r) raw_data = Candidate a v' b r
  where
    v' = updateT v raw_data

updateCandidate :: Candidate -> IO Candidate
updateCandidate = undefined -- TODO

updateCandidates :: Candidates -> IO Candidates
updateCandidates candidates = sequence $ fmap updateCandidate candidates

slice :: Int -> Int -> B.ByteString -> B.ByteString
slice start len = B.take len . B.drop start

-- Extract Candidates from Chunks of memory
extractHelper :: (Type a) => a -> RData -> M.Address -> Candidates
extractHelper helper rdata offset = candidates
  where
    mem_position = (M.startAddress $ reginfo rdata) + offset -- Current position in memory
    end_position = (M.endAddress $ reginfo rdata) -- End of the virtual memory range
    mem_space = end_position - mem_position -- Amount of bytes left in memory
    type_size = size helper -- sizeof(a)
    bytes = slice (fromIntegral offset) type_size (regdata rdata)
    value = updateT helper bytes
    this = if (fromIntegral mem_space) < type_size then [] else [Candidate mem_position value value (reginfo rdata)] -- If there is space left to fit a type a, then get the value. Else, get nothing
    that = if (mem_position + offset > end_position) then [] else extractHelper helper rdata (offset + 1) -- If we hit the end of the memory region, stop. Else, shift one byte and continue
    candidates = this ++ that

extracti8 :: RData -> Candidates
extracti8 rdata = extractHelper (0 :: Int8) rdata 0

extracti16 :: RData -> Candidates
extracti16 rdata = extractHelper (0 :: Int16) rdata 0

extracti32 :: RData -> Candidates
extracti32 rdata = extractHelper (0 :: Int32) rdata 0

extracti64 :: RData -> Candidates
extracti64 rdata = extractHelper (0 :: Int64) rdata 0

extractu8 :: RData -> Candidates
extractu8 rdata = extractHelper (0 :: UInt8) rdata 0

extractu16 :: RData -> Candidates
extractu16 rdata = extractHelper (0 :: UInt16) rdata 0

extractu32 :: RData -> Candidates
extractu32 rdata = extractHelper (0 :: UInt32) rdata 0

extractu64 :: RData -> Candidates
extractu64 rdata = extractHelper (0 :: UInt64) rdata 0

extractF :: RData -> Candidates
extractF rdata = extractHelper (0 :: Float) rdata 0

extractD :: RData -> Candidates
extractD rdata = extractHelper (0 :: Double) rdata 0

debug :: IO ()
debug = do
    let pid = 1012533 :: Int
    all_maps <- M.getMapInfo pid
    let maps = M.filterMap (M.defaultFilter all_maps) all_maps
    putStrLn $ show maps
    putStrLn $ show . length . M.regions $ maps
    return ()
