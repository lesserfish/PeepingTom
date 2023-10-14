module PeepingTom where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder (byteStringHex, toLazyByteString)
import Data.List (intersperse)
import Data.Maybe (Maybe, mapMaybe)
import qualified PeepingTom.Filters as Filters
import qualified PeepingTom.IO as IO
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
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
        ++ printf "Regions: \n\n%s\n\n" rstr
        ++ printf "Candidates: \n\n%s\n\n" cstr
  where
    candidate_list = take maxcandidates (pCandidates ps)
    cstrlist = fmap show candidate_list
    cstr = concat $ intersperse "\n" cstrlist
    region_list = pRegions ps
    rstrlist = fmap show (take maxregions $ Maps.regions region_list)
    rstr = concat $ intersperse "\n" rstrlist

instance Show PeepState where
    show = printPeepState 5 5

slice :: Address -> Address -> BS.ByteString -> BS.ByteString
slice start len = BS.take len . BS.drop start

initialExtractIO' :: Address -> [Type] -> Filter -> IO.RegionData -> IO [Candidate]
initialExtractIO' offset types fltr regdata = do
    let start_address = Maps.startAddress . IO.rInfo $ regdata
    let end_address = Maps.endAddress . IO.rInfo $ regdata
    if start_address + offset > end_address -- If we reached the end of the memory block, stop
        then return []
        else do
            tail <- initialExtractIO' (offset + 1) types fltr regdata
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

debug :: IO ()
debug = do
    let pid = 1436738 :: PID
    putStrLn $ printf "PID: %d" pid
    all_maps <- Maps.getMapInfo pid
    let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
    regs <- IO.loadMap maps
    let reg = regs !! 1
    let fltr = Filters.compareInteger (== 41)
    candidates <- initialExtractIO' 0 [(Type Int32)] fltr reg
    putStrLn $ printList candidates
    return ()
