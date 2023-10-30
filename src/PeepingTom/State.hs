module PeepingTom.State (
    Candidate (..),
    PeepState (..),
    showState,
    candidateCount,
    regionCount,
) where

import qualified Data.ByteString as BS
import Data.List (intersperse)
import PeepingTom.Common
import PeepingTom.Internal
import PeepingTom.Map
import PeepingTom.Type
import Text.Printf (printf)

-- Data types

data Candidate = Candidate
    { cAddress :: Address
    , cData :: BS.ByteString
    , cTypes :: [Type]
    , cRegionID :: Int
    }

data PeepState = PeepState
    { psPID :: PID
    , psCandidates :: [Candidate]
    , psRegions :: MapInfo
    }

instance Show Candidate where
    show candidate =
        printf
            "Address: 0x%08X\tData: [%s]\tPossible Types: %s\tRegion ID: %d"
            (cAddress $ candidate)
            (bsToString $ cData candidate)
            (show . cTypes $ candidate)
            (cRegionID candidate)

instance Show PeepState where
    show = showState 5 5

showState :: Int -> Int -> PeepState -> String
showState maxcandidates' maxregions' ps =
    printf "PID: %d\n" (psPID ps)
        ++ printf "Regions (%d): \n\n%s\n%s\n" (length . miRegions . psRegions $ ps) rstr rdot
        ++ printf "Candidates (%d): \n\n%s\n%s\n" (length . psCandidates $ ps) cstr cdot
  where
    maxcandidates = if maxcandidates' < 0 then (length . psCandidates $ ps) else maxcandidates'
    maxregions = if maxregions' < 0 then (length . miRegions . psRegions $ ps) else maxregions'
    candidate_list = take maxcandidates (psCandidates ps)
    cstrlist = fmap show candidate_list
    cstr = concat $ intersperse "\n" cstrlist
    cdot = if (length . psCandidates $ ps) > maxcandidates then "...\n" else ""
    region_list = psRegions ps
    rstrlist = fmap show (take maxregions $ miRegions region_list)
    rstr = concat $ intersperse "\n" rstrlist
    rdot = if (length . miRegions $ region_list) > maxregions then "...\n" else ""

candidateCount :: PeepState -> Int
candidateCount = length . psCandidates

regionCount :: PeepState -> Int
regionCount = length . miRegions . psRegions
