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

matchFilter :: FilterInfo -> Match -> Maybe Match
matchFilter (fltr, _) match = output
  where
    bsData = mData $ match :: BS.ByteString
    valid_types = fltr bsData :: [Type]
    output = if length valid_types == 0 then Nothing else match'
    max_size = maxSizeOf valid_types
    bsData' = BS.take max_size bsData -- Only store that amount of bytes
    match' = Just match{mData = bsData', mTypes = valid_types}

isInChunk :: Match -> IO.MemoryChunk -> Bool
isInChunk match chunk = (match_addr >= chunk_addr) && (match_addr + max_size <= chunk_addr + chunk_size)
  where
    match_addr = mAddress match
    match_types = mTypes match
    max_size = maxSizeOf match_types
    chunk_addr = IO.mcStartAddr chunk
    chunk_size = IO.mcSize chunk

filterAddress :: FilterInfo -> Int -> IO.MemoryChunk -> Address -> Maybe Match
filterAddress (fltr, _) regid chunk offset = output
  where
    address = (IO.mcStartAddr chunk) + offset
    bytes = BS.drop offset (IO.mcData chunk) -- Get all the bytes starting from offset
    match_types = fltr bytes :: [Type]
    max_size = maxSizeOf match_types
    byte_data = BS.take max_size bytes -- Store this amount of bytes
    region_id = regid
    new_match =
        Match
            { mAddress = address
            , mData = byte_data
            , mTypes = match_types
            , mRegionID = region_id
            }
    output = if null match_types then Nothing else Just new_match

regionScanHelper :: IO.RInterface -> FilterInfo -> Size -> (Address, Address) -> ScanOptions -> IO [Match]
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
                    let matchs = mapMaybe (filterAddress fltr regid chunk) offset
                    evaluate matchs
                    return $ matchs ++ tail
                else do
                    putStrLn $ printf "Failed to load chunk (%8x, %8x) of region %d" start_address (start_address + chunk_size) regid
                    return tail

regionScan :: IO.RInterface -> FilterInfo -> Region -> ScanOptions -> IO [Match]
regionScan rinterface fltr region scopt = do
    let rid = rID region
    let sa = rStartAddr region
    let ea = rEndAddr region
    matchs <- regionScanHelper rinterface fltr rid (sa, ea) scopt
    return matchs

regionScanLog :: Region -> [Match] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d matchs from Region %4d (size = %8x)" (length result) (rID reg) ((rEndAddr reg) - (rStartAddr reg))

scanMapHelper :: ScanOptions -> FilterInfo -> MapInfo -> IO [Match]
scanMapHelper scopt fltr map = do
    let stopsig = soSIGSTOP scopt
    let pid = miPID map
    let regions = miRegions map
    matchs <-
        IO.withRInterface
            pid
            stopsig
            ( \rinterface -> do
                let action = vocal regionScanLog (\x -> regionScan rinterface fltr x scopt) :: Region -> IO [Match]
                fc <- forM regions action :: IO [[Match]]
                return $ concat fc
            )
    return $ matchs

updateMatchsHelper :: IO.RInterface -> [Match] -> IO.MemoryChunk -> IO [Match]
updateMatchsHelper _ [] _ = return []
updateMatchsHelper rinterface (current : rest) memory_chunk = do
    if (isInChunk current memory_chunk)
        then do
            let offset = (mAddress current) - (IO.mcStartAddr memory_chunk) -- Get the offset of the memory location in the bytestring corresponding to address
            let valid_types = mTypes current
            let byte_counts = map sizeOf valid_types
            let byte_count = foldl max 0 byte_counts
            let new_bytes = slice offset byte_count (IO.mcData memory_chunk)
            let new_match = current{mData = new_bytes}
            tail <- updateMatchsHelper rinterface rest memory_chunk
            return $ [new_match] ++ tail
        else do
            let new_chunk_addr = mAddress current
            let chunk_size = IO.mcSize memory_chunk
            new_chunk <- rinterface new_chunk_addr chunk_size
            updateMatchsHelper rinterface (current : rest) new_chunk

updateMatchs :: IO.RInterface -> ScanOptions -> [Match] -> IO [Match]
updateMatchs _ _ [] = return []
updateMatchs rinterface scopt matchs = do
    let chunk_size = soChunkSize scopt
    let initial = matchs !! 0
    let new_chunk_addr = mAddress initial
    chunk <- rinterface new_chunk_addr chunk_size
    updateMatchsHelper rinterface matchs chunk

-- Public Methods

updateStateS :: ScanOptions -> PeepState -> IO PeepState
updateStateS scopt state = do
    let pid = psPID state
    let stopsig = soSIGSTOP scopt
    let matchs = psMatchs state
    let action = (\rinterface -> updateMatchs rinterface scopt matchs) :: IO.RInterface -> IO [Match]
    matchs' <- IO.withRInterface pid stopsig action
    return $ state{psMatchs = matchs'}

updateState :: PeepState -> IO PeepState
updateState = updateStateS (defaultScanOptions)

scanMapS :: ScanOptions -> FilterInfo -> MapInfo -> IO PeepState
scanMapS scopt fltr map = do
    let pid = (miPID map)
    matchs <- scanMapHelper scopt fltr map
    return PeepState{psPID = pid, psMatchs = matchs, psRegions = map}

scanMap :: FilterInfo -> MapInfo -> IO PeepState
scanMap = scanMapS defaultScanOptions
