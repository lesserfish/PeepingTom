module PeepingTom.State where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.List (intersperse, reverse)
import Data.List.Split (chunk)
import Data.Maybe (Maybe, catMaybes, mapMaybe)
import qualified PeepingTom.Conversions as Conversions
import qualified PeepingTom.Filters as Filters
import qualified PeepingTom.IO as IO
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.Posix as Posix
import PeepingTom.Type
import qualified PeepingTom.Writer as Writer
import Text.Printf (printf)

-- Data types

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

instance Show Candidate where
    show candidate =
        printf
            "Address: 0x%08X\tData: [%s]\tPossible Types: %s\tRegion ID: %d"
            (cAddress $ candidate)
            (bsToHex $ cData candidate)
            (show . cTypes $ candidate)
            (cRegionID candidate)

instance Show PeepState where
    show = showState 5 5

-- Private Methods

bsToHex :: BS.ByteString -> String
bsToHex bs = concat $ intersperse " " (fmap (printf "0x%02X") (BS.unpack bs))

printList :: (Show a) => [a] -> String
printList lst = concat $ intersperse "\n" (fmap show lst)

slice :: Address -> Address -> BS.ByteString -> BS.ByteString
slice start len = BS.take len . BS.drop start

vocal :: (a -> b -> IO ()) -> (a -> IO b) -> a -> (IO b)
vocal log action input = do
    output <- action input
    log input output
    return output

maxSizeOf :: [Type] -> Size
maxSizeOf types = foldr max 0 (map sizeOf types)

maxType :: [Type] -> Type
maxType [] = (Type Void)
maxType (t : rest)
    | sizeOf t < sizeOf (maxType rest) = (maxType rest)
    | otherwise = t

filterRegion :: [Type] -> Filters.Filter -> IO.RegionData -> Address -> Maybe Candidate
filterRegion types fltr regdata offset = output
  where
    address = (Maps.startAddress . IO.rInfo $ regdata) + offset
    bytes = BS.drop offset (IO.rData regdata) -- Get all the bytes starting from offset
    candidate_types = filter (fltr bytes) types :: [Type]
    max_size = maxSizeOf candidate_types
    byte_data = BS.take max_size bytes -- Store this amount of bytes
    region_id = Maps.regionID . IO.rInfo $ regdata
    new_candidate =
        Candidate
            { cAddress = address
            , cData = byte_data
            , cTypes = candidate_types
            , cRegionID = region_id
            }
    output = if length candidate_types == 0 then Nothing else Just new_candidate

regionScan :: [Type] -> Filters.Filter -> IO.RegionData -> [Candidate]
regionScan types fltr regdata = mapMaybe (filterRegion types fltr regdata) offsets
  where
    offsets = [0 .. (end_addr - start_addr)]
    start_addr = (Maps.startAddress . IO.rInfo $ regdata)
    end_addr = (Maps.endAddress . IO.rInfo $ regdata)

scanMap' :: [Type] -> Filters.Filter -> Maps.MapInfo -> IO [Candidate]
scanMap' types fltr map = do
    regions <- IO.loadMap map
    let action = vocal regionScanLog (\x -> return $ regionScan types fltr x)
    fc <- forM regions action :: IO [[Candidate]]
    return $ concat fc

regionScanLog :: IO.RegionData -> [Candidate] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d candidates from Region %d" (length result) (Maps.regionID . IO.rInfo $ reg)

candidateFilter :: Filters.Filter -> Candidate -> Maybe Candidate
candidateFilter fltr candidate = output
  where
    bsData = cData $ candidate :: BS.ByteString
    valid_types = filter (fltr bsData) (cTypes candidate) :: [Type]
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
    chunk_addr = IO.startAddress chunk
    chunk_size = IO.chunkSize chunk

updateCandidates' :: IO.CInterface -> [Candidate] -> IO.MemoryChunk -> IO [Candidate]
updateCandidates' _ [] _ = return []
updateCandidates' cinterface (current : rest) memory_chunk = do
    if (isInChunk current memory_chunk)
        then do
            let offset = (cAddress current) - (IO.startAddress memory_chunk) -- Get the offset of the memory location in the bytestring corresponding to address
            let valid_types = cTypes current
            let byte_counts = map sizeOf valid_types
            let byte_count = foldl max 0 byte_counts
            let new_bytes = slice offset byte_count (IO.mData memory_chunk)
            let new_candidate = current{cData = new_bytes}
            tail <- updateCandidates' cinterface rest memory_chunk
            return $ [new_candidate] ++ tail
        else do
            let new_chunk_addr = cAddress current
            let chunk_size = IO.chunkSize memory_chunk
            new_chunk <- cinterface new_chunk_addr chunk_size
            updateCandidates' cinterface (current : rest) new_chunk

updateCandidates :: IO.CInterface -> Int -> [Candidate] -> IO [Candidate]
updateCandidates _ _ [] = return []
updateCandidates cinterface chunk_size candidates = do
    let initial = candidates !! 0
    let new_chunk_addr = cAddress initial
    chunk <- cinterface new_chunk_addr chunk_size
    updateCandidates' cinterface candidates chunk

writeCandidate :: IO.WInterface -> Writer.Writer -> Candidate -> IO Candidate
writeCandidate winterface writer candidate = do
    let addr = cAddress candidate
    let maxtype = maxType . cTypes $ candidate -- Get the type with the largest size
    let bs_data = writer maxtype -- Get the bytes to write
    let data_size = BS.length bs_data
    if data_size == 0
        then return candidate -- Writer does not support this type; Don't write anything
        else do
            winterface addr bs_data
            let candidate' = candidate{cData = bs_data}
            return candidate'

applyWriter' :: IO.WInterface -> Writer.Writer -> [Candidate] -> IO [Candidate]
applyWriter' winterface writer candidates = sequence $ map (writeCandidate winterface writer) candidates

-- Public Methods

showState :: Int -> Int -> PeepState -> String
showState maxcandidates maxregions ps =
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

applyFilter :: Filters.Filter -> PeepState -> PeepState
applyFilter fltr state = output
  where
    candidates' = mapMaybe (candidateFilter fltr) (pCandidates state)
    output = state{pCandidates = candidates'}

updateState :: Size -> PeepState -> IO PeepState
updateState chunk_size state = do
    let pid = pPID state
    let candidates = pCandidates state
    let action = (\cinterface -> updateCandidates cinterface chunk_size candidates) :: IO.CInterface -> IO [Candidate]
    candidates' <- IO.withCInterface pid action
    return $ state{pCandidates = candidates'}

scanMap :: [Type] -> Filters.Filter -> Maps.MapInfo -> IO PeepState
scanMap types fltr map = do
    let pid = (Maps.mPID map)
    candidates <- scanMap' types fltr map
    return PeepState{pPID = pid, pCandidates = candidates, pRegions = map}

applyWriter :: Writer.Writer -> PeepState -> IO PeepState
applyWriter writer peepstate = do
    let pid = pPID peepstate
    let candidates = pCandidates peepstate
    let action = (\winterface -> applyWriter' winterface writer candidates) :: (IO.WInterface -> IO [Candidate])
    candidates' <- IO.withWInterface pid action
    return $ peepstate{pCandidates = candidates'}

debug :: IO ()
debug = do
    let pid = 1436738 :: PID
    putStrLn $ printf "PID: %d" pid
    all_maps <- Maps.getMapInfo pid
    let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
    let fltr_41 = Filters.eqInteger 41
    peepstate <- scanMap [(Type Int64)] fltr_41 maps
    putStrLn $ "[DEBUG] This is the initial extraction of Int64 Types equal to 41"
    putStrLn $ "[DEBUG] Please compare this result with scanmem!"
    putStrLn $ showState 5 0 peepstate
    putStrLn $ "[DEBUG] After the results have been checked, please alter the candidates 0..200 to value 43! Please enter once complete...."
    _ <- getChar
    putStrLn $ "[DEBUG] We are now showing the candidates which still have value equal 41. Please compare the results with scanmem!"
    peepstate2 <- updateState 4096 peepstate
    let peepstate41 = applyFilter fltr_41 peepstate2
    putStrLn $ showState 5 0 peepstate41
    putStrLn $ "[DEBUG] We are now showing the candidates which have a value equal to 43. Please compare the results with scanmem! Please enter once complete!"
    let peepstate43 = applyFilter (Filters.eqInteger 43) peepstate2
    putStrLn $ showState 5 0 peepstate43
    _ <- getChar
    putStrLn $ "[DEBUG] We are now updating the candidates with value equal to 43. We are setting the new value equal to 45! Press enter to continue:"
    _ <- getChar
    peepstate45 <- applyWriter (Writer.writeInt 45) peepstate43
    putStrLn $ "[DEBUG] This is the result after updating the values! Please compare the results with scanmem!"
    putStrLn $ showState 5 0 peepstate45
    _ <- getChar
    putStrLn $ "[DEBUG] This is the end of the Debug! Please set all of the remaining candidates in scanmem equal to 41. Hope it worked out!"
    _ <- getChar
    return ()
