{-# LANGUAGE GADTs #-}

module PeepingTom where

import Control.Monad.State
import qualified Data.ByteString as B
import Internal
import qualified Maps as M
import Text.Printf (printf)
import Type
import TypeFilter
import qualified VMIO

data Candidate = forall a. (Show a, Type a) => Candidate {caddress :: Address, cvalue :: a, pvalue :: a, cregion :: M.Region}
data PeepingState = PeepingState {candidateList :: [Candidate], cPID :: PID}

instance Show Candidate where
    show (Candidate addr cv pv reg) =
        "Address: "
            ++ printf "%x" addr
            ++ "\t Value: "
            ++ showT cv
            ++ "\t Previous Value: "
            ++ showT pv
            ++ "\t Region ID: "
            ++ show (M.regionID reg)

-- Helper functions regarding PeepingState
showPeepingState' :: [Candidate] -> String
showPeepingState' [] = ""
showPeepingState' (y : ys) = show y ++ "\n" ++ showPeepingState' ys

showPeepingState :: PeepingState -> String
showPeepingState = showPeepingState' . candidateList

updateValue :: Candidate -> B.ByteString -> Candidate
updateValue (Candidate a v b r) raw_data = Candidate a v' b r
  where
    v' = updateT v raw_data

-- Extractors:
--
-- These take a chunk of memory (in the form of VMIO.RData), and extract all possible
-- peeping_state.
--
-- The result is a VERY large list of unfiltered peeping_state.
-- You absolutely SHOULD filter the results before doing anything with them.
--
--
slice :: Int -> Int -> B.ByteString -> B.ByteString
slice start len = B.take len . B.drop start

{-
extractHelper :: (Type a) => a -> VMIO.RData -> Address -> [Candidate]
extractHelper helper rdata offset = peeping_state
  where
    mem_position = (M.startAddress $ VMIO.reginfo rdata) + offset -- Current position in memory
    end_position = (M.endAddress $ VMIO.reginfo rdata) -- End of the virtual memory range
    mem_space = end_position - mem_position -- Amount of bytes left in memory
    type_size = size helper -- sizeof(a)
    bytes = slice (fromIntegral offset) (fromIntegral type_size) (VMIO.regdata rdata) -- memory[mem_position : mem_position + type_size]
    value = updateT helper bytes -- Equivalent to casting the previouls bytes to a
    this = if (fromIntegral mem_space) < type_size then [] else [Candidate (fromIntegral mem_position) value value (VMIO.reginfo rdata)] -- If there is space left to fit a type a, then get the value. Else, get nothing
    that = if (mem_position + offset > end_position) then [] else extractHelper helper rdata (offset + 1) -- If we hit the end of the memory region, stop. Else, shift one byte and continue
    peeping_state = this ++ that
-}

{-
extractHelper :: (Type a) => a -> VMIO.RData -> Address -> [Candidate]
extractHelper helper rdata offset = peeping_state
  where
    mem_position = (M.startAddress $ VMIO.reginfo rdata) + offset -- Current position in memory
    end_position = (M.endAddress $ VMIO.reginfo rdata) -- End of the virtual memory range
    mem_space = end_position - mem_position -- Amount of bytes left in memory
    type_size = size helper -- sizeof(a)
    bytes = slice (fromIntegral offset) (fromIntegral type_size) (VMIO.regdata rdata) -- memory[mem_position : mem_position + type_size]
    value = updateT helper bytes -- Equivalent to casting the previouls bytes to a
    this = [Candidate (fromIntegral mem_position) value value (VMIO.reginfo rdata)] -- If there is space left to fit a type a, then get the value. Else, get nothing
    that = if (mem_position + (fromIntegral type_size) >= end_position) then [] else extractHelper helper rdata (offset + 1) -- If we hit the end of the memory region, stop. Else, shift one byte and continue
    peeping_state' = this ++ that
    bytehex = BB.toLazyByteString . BB.byteStringHex $ bytes
    debug1 = show (VMIO.reginfo rdata) ++ printf "Position: %x; Bytes Left: %x; Bytes Read: %s; Value: %s" mem_position mem_space (show bytehex) (showT value)
    st = 0x0000
    ln = 0x10000000
    peeping_state = if offset > st && offset < (st + ln) then trace debug1 peeping_state' else peeping_state'
-}

extractHelper :: (Type a) => a -> VMIO.RData -> Address -> [Candidate]
extractHelper helper rdata offset = peeping_state
  where
    mem_position = (M.startAddress $ VMIO.reginfo rdata) + offset -- Current position in memory
    end_position = (M.endAddress $ VMIO.reginfo rdata) -- End of the virtual memory range
    type_size = size helper -- sizeof(a)
    bytes = slice (fromIntegral offset) (fromIntegral type_size) (VMIO.regdata rdata) -- memory[mem_position : mem_position + type_size]
    value = updateT helper bytes -- Equivalent to casting the previouls bytes to a
    peeping_state = this ++ that
    this = [Candidate (fromIntegral mem_position) value value (VMIO.reginfo rdata)]
    that =
        if (mem_position + (fromIntegral type_size) >= end_position) -- If we hit the end of the memory region, stop. Else, shift one byte and continue
            then []
            else extractHelper helper rdata (offset + 1)

(~>) :: (a -> State b c) -> (a -> State b d) -> (a -> State b d)
f1 ~> f2 = \x -> f1 x >>= (\_ -> f2 x)

type Extractor = VMIO.RData -> State [Candidate] ()

extracti8 :: Extractor
extracti8 rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: Int8) rdata 0
    put (peeping_state ++ new_peeping_state)

extracti16 :: Extractor
extracti16 rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: Int16) rdata 0
    put (peeping_state ++ new_peeping_state)

extracti32 :: Extractor
extracti32 rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: Int32) rdata 0
    put (peeping_state ++ new_peeping_state)

extracti64 :: Extractor
extracti64 rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: Int64) rdata 0
    put (peeping_state ++ new_peeping_state)

extractu8 :: Extractor
extractu8 rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: UInt8) rdata 0
    put (peeping_state ++ new_peeping_state)

extractu16 :: Extractor
extractu16 rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: UInt16) rdata 0
    put (peeping_state ++ new_peeping_state)

extractu32 :: Extractor
extractu32 rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: UInt32) rdata 0
    put (peeping_state ++ new_peeping_state)

extractu64 :: Extractor
extractu64 rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: UInt64) rdata 0
    put (peeping_state ++ new_peeping_state)

extractf :: Extractor
extractf rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: Float) rdata 0
    put (peeping_state ++ new_peeping_state)

extractd :: Extractor
extractd rdata = do
    peeping_state <- get
    let new_peeping_state = extractHelper (0 :: Double) rdata 0
    put (peeping_state ++ new_peeping_state)

extractInt :: Extractor
extractInt =
    extracti8
        ~> extracti16
        ~> extracti32
        ~> extracti64
        ~> extractu8
        ~> extractu16
        ~> extractu32
        ~> extractu64

extractFloat :: Extractor
extractFloat = extractf ~> extractd

extractNum :: Extractor
extractNum = extractInt ~> extractFloat

applyExtractor :: Extractor -> VMIO.RData -> [Candidate]
applyExtractor extractor memory_block = execState (extractor memory_block) []

mApplyExtractor :: Extractor -> [VMIO.RData] -> [Candidate]
mApplyExtractor extractor blocks = concat $ fmap (applyExtractor extractor) blocks

readMap :: M.MapInfo -> Extractor -> IO PeepingState
readMap mapinfo extractor = do
    memory_blocks <- VMIO.loadMap mapinfo :: IO [VMIO.RData]
    let clist = mApplyExtractor extractor memory_blocks :: [Candidate]
    let peeping_state = PeepingState{candidateList = clist, cPID = M.mPID mapinfo}
    return peeping_state

-- Update PeepingState

updateCandidate :: VMIO.RInterface -> Candidate -> IO Candidate
updateCandidate rinterface (Candidate addr cval _ reg) = do
    let byte_count = size cval -- Sizeof underlying type
    bytes <- rinterface addr byte_count -- Read the amount of bytes at the given address
    let updated_value = updateT cval bytes -- Cast the bytes to the type
    return $ Candidate addr updated_value cval reg -- Return the updated candidate values

updatePeepingState' :: PeepingState -> VMIO.RInterface -> IO PeepingState
updatePeepingState' peeping_state rinterface = do
    let clist = candidateList peeping_state :: [Candidate]
    clist' <- sequence $ fmap (updateCandidate rinterface) clist :: IO [Candidate]
    return peeping_state{candidateList = clist'}

updatePeepingState :: PeepingState -> IO PeepingState
updatePeepingState peeping_state = do
    let pid = cPID peeping_state
    VMIO.withReadInterface pid (updatePeepingState' peeping_state)

-- Filter PeepingState

filterNines' :: Integer -> Candidate -> Bool
filterNines' i (Candidate _ v _ _) = compareInt (== i) v

filterNines :: Integer -> PeepingState -> PeepingState
filterNines i peeping_state = peeping_state{candidateList = newlist}
  where
    newlist = filter (filterNines' i) (candidateList peeping_state)

removeRegions :: M.MapInfo -> M.MapInfo
removeRegions mapinfo = mapinfo{M.regions = justfirst}
  where
    justfirst = [(M.regions mapinfo) !! 1]

debug :: IO ()
debug = do
    let pid = 1012533 :: PID
    all_maps <- M.getMapInfo pid
    let maps = M.filterMap (M.defaultFilter all_maps) all_maps
    let maps' = removeRegions maps
    putStrLn $ show maps'
    putStrLn $ show . length . M.regions $ maps'
    peeping_state <- readMap maps' extracti32
    putStrLn $ printf "There are %d i32s in the memory block." (length . candidateList $ peeping_state)
    let i = 27 :: Integer
    let justnines = filterNines i peeping_state
    putStrLn $ printf "%d of those are equal to %d" (length . candidateList $ justnines) i
    putStrLn $ showPeepingState justnines
    return ()
