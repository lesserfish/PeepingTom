module PeepingTom.State where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder (byteStringHex, toLazyByteString)
import Data.List (intersperse, reverse)
import Data.List.Split (chunk)
import Data.Maybe (Maybe, mapMaybe)
import qualified PeepingTom.Conversions as Conversions
import qualified PeepingTom.Filters as Filters
import qualified PeepingTom.IO as IO
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.Posix as Posix
import PeepingTom.Type
import Text.Printf (printf)

data Candidate = Candidate
    { cAddress :: Address
    , cData :: BS.ByteString
    , cTypes :: [Type]
    , cRegionID :: Int
    }

data PeepState = PeepState
    { pPID :: PID
    , pCandidates :: [Candidate]
    , pRegions :: Maps.MapInfo
    }

bsToHex :: BS.ByteString -> String
bsToHex bs = concat $ intersperse " " (fmap (printf "0x%02X") (BS.unpack bs))

instance Show Candidate where
    show candidate =
        printf
            "Address: 0x%08X\tData: [%s]\tPossible Types: %s\tRegion ID: %d"
            (cAddress $ candidate)
            (bsToHex $ cData candidate)
            (show . cTypes $ candidate)
            (cRegionID candidate)

printList :: (Show a) => [a] -> String
printList lst = concat $ intersperse "\n" (fmap show lst)

printPeepState :: Int -> Int -> PeepState -> String
printPeepState maxcandidates maxregions ps =
    printf "PID: %d\n" (pPID ps)
        ++ printf "Regions (%d): \n\n%s\n%s\n" (length . Maps.regions . pRegions $ ps) rstr rdot
        ++ printf "Candidates (%d): \n\n%s\n%s\n" (length . pCandidates $ ps) cstr cdot
  where
    candidate_list = take maxcandidates (pCandidates ps)
    cstrlist = fmap show candidate_list
    cstr = concat $ intersperse "\n" cstrlist
    cdot = if (length . pCandidates $ ps) > maxcandidates then "...\n" else ""
    region_list = pRegions ps
    rstrlist = fmap show (take maxregions $ Maps.regions region_list)
    rstr = concat $ intersperse "\n" rstrlist
    rdot = if (length . Maps.regions $ region_list) > maxregions then "...\n" else ""

instance Show PeepState where
    show = printPeepState 5 5

slice :: Address -> Address -> BS.ByteString -> BS.ByteString
slice start len = BS.take len . BS.drop start

vocal :: (a -> b -> IO ()) -> (a -> IO b) -> a -> (IO b)
vocal log action input = do
    output <- action input
    log input output
    return output

regionScan' :: Address -> [Type] -> Filter -> IO.RegionData -> [Candidate]
regionScan' offset types fltr regdata = output
  where
    start_address = Maps.startAddress . IO.rInfo $ regdata
    end_address = Maps.endAddress . IO.rInfo $ regdata
    output = if start_address + offset > end_address then [] else output'
    tail = regionScan' (offset + 1) types fltr regdata
    bytes = BS.drop offset (IO.rData regdata) -- Get all the bytes starting from offset
    candidate_types = filter (fltr bytes) types :: [Type]
    output' = if length candidate_types == 0 then tail else output''
    byte_counts = map sizeOf candidate_types :: [Size] -- Get the list of how much bytes each accepted type needs
    max_byte = foldr max 0 byte_counts :: Size -- Gets the largest of them
    byte_data = BS.take max_byte bytes -- Store this amount of bytes
    region_id = Maps.regionID . IO.rInfo $ regdata
    new_candidate =
        Candidate
            { cAddress = start_address + offset
            , cData = byte_data
            , cTypes = candidate_types
            , cRegionID = region_id
            }
    output'' = tail ++ [new_candidate]

regionScan :: [Type] -> Filter -> IO.RegionData -> [Candidate]
regionScan types fltr map = reverse $ regionScan' 0 types fltr map

-- TODO: Remove this???
regionScanIO' :: Address -> [Type] -> Filter -> IO.RegionData -> IO [Candidate]
regionScanIO' offset types fltr regdata = do
    let start_address = Maps.startAddress . IO.rInfo $ regdata
    let end_address = Maps.endAddress . IO.rInfo $ regdata
    if start_address + offset > end_address -- If we reached the end of the memory block, stop
        then return []
        else do
            tail <- regionScanIO' (offset + 1) types fltr regdata
            let bytes = BS.drop offset (IO.rData regdata) -- Get all the bytes starting from offset
            let candidate_types = filter (fltr bytes) types :: [Type]
            if length candidate_types == 0 -- If no type accepts the bytes, return nothing
                then return tail
                else do
                    let byte_counts = map sizeOf candidate_types :: [Int] -- Get the list of how much bytes each accepted type needs
                    let max_byte = foldr max 0 byte_counts :: Int -- Gets the largest of them
                    let byte_data = BS.take max_byte bytes -- Store this amount of bytes
                    let region_id = Maps.regionID . IO.rInfo $ regdata
                    let new_candidate =
                            Candidate
                                { cAddress = start_address + offset
                                , cData = byte_data
                                , cTypes = candidate_types
                                , cRegionID = region_id
                                }
                    return $ [new_candidate] ++ tail

regionScanIO :: [Type] -> Filter -> IO.RegionData -> IO [Candidate]
regionScanIO = regionScanIO' 0

regionScanLog :: IO.RegionData -> [Candidate] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d candidates from Region %d" (length result) (Maps.regionID . IO.rInfo $ reg)

scanMapIO' :: [Type] -> Filter -> Maps.MapInfo -> IO [Candidate]
scanMapIO' types fltr map = do
    regions <- IO.loadMap map
    let action = vocal regionScanLog (regionScanIO types fltr)
    fc <- forM regions action :: IO [[Candidate]]
    return $ concat fc

scanMapIO :: [Type] -> Filter -> Maps.MapInfo -> IO PeepState
scanMapIO types fltr map = do
    let pid = (Maps.mPID map)
    candidates <- scanMapIO' types fltr map
    return PeepState{pPID = pid, pCandidates = candidates, pRegions = map}

scanMap' :: [Type] -> Filter -> Maps.MapInfo -> IO [Candidate]
scanMap' types fltr map = do
    regions <- IO.loadMap map
    let action = vocal regionScanLog (\x -> return $ regionScan types fltr x)
    fc <- forM regions action :: IO [[Candidate]]
    return $ concat fc

scanMap :: [Type] -> Filter -> Maps.MapInfo -> IO PeepState
scanMap types fltr map = do
    let pid = (Maps.mPID map)
    candidates <- scanMap' types fltr map
    return PeepState{pPID = pid, pCandidates = candidates, pRegions = map}

candidateFilter :: Filter -> Candidate -> Maybe Candidate
candidateFilter fltr candidate = output
  where
    bsData = cData $ candidate :: BS.ByteString
    valid_types = filter (fltr bsData) (cTypes candidate) :: [Type]
    output = if length valid_types == 0 then Nothing else candidate'
    byte_counts = map sizeOf valid_types :: [Size] -- Get the list of how much bytes each accepted type needs
    max_byte = foldr max 0 byte_counts :: Size -- Gets the largest of them
    bsData' = BS.take max_byte bsData -- Only store that amount of bytes
    candidate' = Just candidate{cData = bsData', cTypes = valid_types}

applyFilter :: Filter -> PeepState -> PeepState
applyFilter fltr state = output
  where
    candidates' = mapMaybe (candidateFilter fltr) (pCandidates state)
    output = state{pCandidates = candidates'}

isInChunk :: Candidate -> IO.MemoryChunk -> Bool
isInChunk candidate chunk = (candidate_addr >= chunk_addr) && (candidate_addr + max_byte <= chunk_addr + chunk_size)
  where
    candidate_addr = cAddress candidate
    candidate_types = cTypes candidate
    byte_counts = map sizeOf candidate_types :: [Size] -- Get the list of how much bytes each accepted type needs
    max_byte = foldr max 0 byte_counts :: Size -- Gets the largest of them
    chunk_addr = IO.startAddress chunk
    chunk_size = IO.chunkSize chunk

updateCandidates' :: FD -> [Candidate] -> IO.MemoryChunk -> IO [Candidate]
updateCandidates' _ [] _ = return []
updateCandidates' fd (current : rest) memory_chunk = do
    if (isInChunk current memory_chunk)
        then do
            let offset = (cAddress current) - (IO.startAddress memory_chunk) -- Get the offset of the memory location in the bytestring corresponding to address
            let valid_types = cTypes current
            let byte_counts = map sizeOf valid_types
            let byte_count = foldl max 0 byte_counts
            let new_bytes = slice offset byte_count (IO.mData memory_chunk)
            let new_candidate = current{cData = new_bytes}
            tail <- updateCandidates' fd rest memory_chunk
            return $ [new_candidate] ++ tail
        else do
            let new_chunk_addr = cAddress current
            let chunk_size = IO.chunkSize memory_chunk
            new_chunk <- IO.loadMemoryChunk fd new_chunk_addr chunk_size
            updateCandidates' fd (current : rest) new_chunk

updateCandidates :: FD -> Int -> [Candidate] -> IO [Candidate]
updateCandidates _ _ [] = return []
updateCandidates fd chunk_size candidates = do
    let initial = candidates !! 0
    let new_chunk_addr = cAddress initial
    chunk <- IO.loadMemoryChunk fd new_chunk_addr chunk_size
    updateCandidates' fd candidates chunk

updateState :: Size -> PeepState -> IO PeepState
updateState chunk_size state = do
    let pid = pPID state
    let candidates = pCandidates state
    fd <- IO.attach pid Posix.O_RDONLY
    candidates' <- updateCandidates fd chunk_size candidates
    IO.dettach (pid, fd)
    return $ state{pCandidates = candidates'}

debug :: IO ()
debug = do
    let pid = 1436738 :: PID
    putStrLn $ printf "PID: %d" pid
    all_maps <- Maps.getMapInfo pid
    let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
    -- let fltr = Filters.compareInteger (== 41)
    let fltr = Filters.eqInteger 41
    peepstate <- scanMap [(Type Int64)] fltr maps
    putStrLn $ show peepstate
    c <- getChar
    putStrLn $ "Showing candidates equal to 41"
    peepstate' <- updateState 4096 peepstate
    let outstate = applyFilter fltr peepstate'
    putStrLn $ show outstate
    putStrLn $ "Showing candidates equal to 43"
    let outstate = applyFilter (Filters.eqInteger 43) peepstate'
    putStrLn $ show outstate
    return ()
