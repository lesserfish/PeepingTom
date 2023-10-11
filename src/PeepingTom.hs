{-# LANGUAGE GADTs #-}

module PeepingTom where

import qualified Data.ByteString as B
import qualified Maps as M
import Types
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

extracti32 :: RData -> Candidates
extracti32 rdata = extractHelper (0 :: Int32) rdata 0

debug :: IO ()
debug = do
    let reg = M.Region 0xAF (0xAF + 0x06) (M.Permission M.R M.W M.X M.S) 0 (M.MapID 0 0 0) "" 0 0
    let raw_data = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05]
    let bs_data = B.pack raw_data
    let rdata = RData bs_data reg
    let candidates = extracti32 rdata
    putStrLn $ showCandidates candidates
    return ()

filterEq :: Integer -> Candidate -> Bool
filterEq y (Candidate _ x _ _) = (fromInteger y) == x

debug2 :: IO ()
debug2 = do
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
    let fcandidates = filter (filterEq 11) candidates
    putStrLn $ showCandidates fcandidates
    return ()
