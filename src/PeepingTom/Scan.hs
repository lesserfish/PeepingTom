module PeepingTom.Scan (
    ScanOptions (..),
    defaultScanOptions,
    updateState,
    updateStateS,
    scanMapS,
    scanMap,
) where

import Control.Exception
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.List (intersperse, reverse)
import Data.List.Split (chunk)
import Data.Maybe (Maybe, catMaybes, mapMaybe)
import PeepingTom.Common
import PeepingTom.Filter
import qualified PeepingTom.IO as IO
import PeepingTom.Internal
import PeepingTom.Map
import PeepingTom.State
import PeepingTom.Type
import Text.Printf (printf)

-- Data types

data ScanOptions = ScanOptions
    { soChunkSize :: Size
    , soSIGSTOP :: Bool
    }
    deriving (Show)

defaultScanOptions :: ScanOptions
defaultScanOptions = ScanOptions 10000 True

-- Private Methods

candidateFilter :: FilterInfo -> Candidate -> Maybe Candidate
candidateFilter (fltr, _) candidate = output
  where
    bsData = cData $ candidate :: BS.ByteString
    valid_types = fltr bsData :: [Type]
    output = if length valid_types == 0 then Nothing else candidate'
    max_size = maxSizeOf valid_types
    bsData' = BS.take max_size bsData -- Only store that amount of bytes
    candidate' = Just candidate{cData = bsData', cTypes = valid_types}

isInChunk :: Candidate -> IO.MemoryChunk -> Bool
isInChunk candidate chunk = (candidate_addr >= chunk_addr) && (candidate_addr + max_size <= chunk_addr + chunk_size)
  where
    candidate_addr = cAddress candidate
    candidate_types = cTypes candidate
    max_size = maxSizeOf candidate_types
    chunk_addr = IO.mcStartAddr chunk
    chunk_size = IO.mcSize chunk

filterAddress :: FilterInfo -> Int -> IO.MemoryChunk -> Address -> Maybe Candidate
filterAddress (fltr, _) regid chunk offset = output
  where
    address = (IO.mcStartAddr chunk) + offset
    bytes = BS.drop offset (IO.mcData chunk) -- Get all the bytes starting from offset
    candidate_types = fltr bytes :: [Type]
    max_size = maxSizeOf candidate_types
    byte_data = BS.take max_size bytes -- Store this amount of bytes
    region_id = regid
    new_candidate =
        Candidate
            { cAddress = address
            , cData = byte_data
            , cTypes = candidate_types
            , cRegionID = region_id
            }
    output = if null candidate_types then Nothing else Just new_candidate

regionScanHelper :: IO.RInterface -> FilterInfo -> Size -> (Address, Address) -> ScanOptions -> IO [Candidate]
regionScanHelper rinterface fltr regid (start_address, end_address) scopt = do
    if start_address >= end_address
        then return []
        else do
            let chunk_size = soChunkSize scopt
            let (_, max_size) = fltr
            let offset_size = min (end_address - start_address) chunk_size
            let read_size = min (end_address - start_address) (chunk_size + max_size)
            let offset = [0 .. offset_size]
            tail <- regionScanHelper rinterface fltr regid (start_address + offset_size + 1, end_address) scopt
            chunk <- rinterface start_address read_size
            if IO.mcOk chunk
                then do
                    let candidates = mapMaybe (filterAddress fltr regid chunk) offset
                    evaluate candidates
                    return $ candidates ++ tail
                else do
                    putStrLn $ printf "Failed to load chunk (%8x, %8x) of region %d" start_address (start_address + chunk_size) regid
                    return tail

regionScan :: IO.RInterface -> FilterInfo -> Region -> ScanOptions -> IO [Candidate]
regionScan rinterface fltr region scopt = do
    let rid = rID region
    let sa = rStartAddr region
    let ea = rEndAddr region
    candidates <- regionScanHelper rinterface fltr rid (sa, ea) scopt
    return candidates

regionScanLog :: Region -> [Candidate] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d candidates from Region %4d (size = %8x)" (length result) (rID reg) ((rEndAddr reg) - (rStartAddr reg))

scanMapHelper :: ScanOptions -> FilterInfo -> MapInfo -> IO [Candidate]
scanMapHelper scopt fltr map = do
    let stopsig = soSIGSTOP scopt
    let pid = miPID map
    let regions = miRegions map
    candidates <-
        IO.withRInterface
            pid
            stopsig
            ( \rinterface -> do
                let action = vocal regionScanLog (\x -> regionScan rinterface fltr x scopt) :: Region -> IO [Candidate]
                fc <- forM regions action :: IO [[Candidate]]
                return $ concat fc
            )
    return $ candidates

updateCandidatesHelper :: IO.RInterface -> [Candidate] -> IO.MemoryChunk -> IO [Candidate]
updateCandidatesHelper _ [] _ = return []
updateCandidatesHelper rinterface (current : rest) memory_chunk = do
    if (isInChunk current memory_chunk)
        then do
            let offset = (cAddress current) - (IO.mcStartAddr memory_chunk) -- Get the offset of the memory location in the bytestring corresponding to address
            let valid_types = cTypes current
            let byte_counts = map sizeOf valid_types
            let byte_count = foldl max 0 byte_counts
            let new_bytes = slice offset byte_count (IO.mcData memory_chunk)
            let new_candidate = current{cData = new_bytes}
            tail <- updateCandidatesHelper rinterface rest memory_chunk
            return $ [new_candidate] ++ tail
        else do
            let new_chunk_addr = cAddress current
            let chunk_size = IO.mcSize memory_chunk
            new_chunk <- rinterface new_chunk_addr chunk_size
            updateCandidatesHelper rinterface (current : rest) new_chunk

updateCandidates :: IO.RInterface -> ScanOptions -> [Candidate] -> IO [Candidate]
updateCandidates _ _ [] = return []
updateCandidates rinterface scopt candidates = do
    let chunk_size = soChunkSize scopt
    let initial = candidates !! 0
    let new_chunk_addr = cAddress initial
    chunk <- rinterface new_chunk_addr chunk_size
    updateCandidatesHelper rinterface candidates chunk

-- Public Methods

updateStateS :: ScanOptions -> PeepState -> IO PeepState
updateStateS scopt state = do
    let pid = psPID state
    let stopsig = soSIGSTOP scopt
    let candidates = psCandidates state
    let action = (\rinterface -> updateCandidates rinterface scopt candidates) :: IO.RInterface -> IO [Candidate]
    candidates' <- IO.withRInterface pid stopsig action
    return $ state{psCandidates = candidates'}

updateState :: PeepState -> IO PeepState
updateState = updateStateS (defaultScanOptions)

scanMapS :: ScanOptions -> FilterInfo -> MapInfo -> IO PeepState
scanMapS scopt fltr map = do
    let pid = (miPID map)
    candidates <- scanMapHelper scopt fltr map
    return PeepState{psPID = pid, psCandidates = candidates, psRegions = map}

scanMap :: FilterInfo -> MapInfo -> IO PeepState
scanMap = scanMapS defaultScanOptions
