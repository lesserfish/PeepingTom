module PeepingTom.State (
    Match (..),
    PeepState (..),
    showState,
    matchCount,
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

data Match = Match
    { mAddress :: Address
    , mData :: BS.ByteString
    , mTypes :: [Type]
    , mRegionID :: Int
    }

data PeepState = PeepState
    { psPID :: PID
    , psMatchs :: [Match]
    , psRegions :: MapInfo
    }

instance Show Match where
    show match =
        printf
            "Address: 0x%08X\tData: [%s]\tPossible Types: %s\tRegion ID: %d"
            (mAddress $ match)
            (bsToString $ mData match)
            (show . mTypes $ match)
            (mRegionID match)

instance Show PeepState where
    show = showState 5 5

showState :: Int -> Int -> PeepState -> String
showState maxmatchs' maxregions' ps =
    printf "PID: %d\n" (psPID ps)
        ++ printf "Regions (%d): \n\n%s\n%s\n" (length . miRegions . psRegions $ ps) rstr rdot
        ++ printf "Matchs (%d): \n\n%s\n%s\n" (length . psMatchs $ ps) cstr cdot
  where
    maxmatchs = if maxmatchs' < 0 then (length . psMatchs $ ps) else maxmatchs'
    maxregions = if maxregions' < 0 then (length . miRegions . psRegions $ ps) else maxregions'
    match_list = take maxmatchs (psMatchs ps)
    cstrlist = fmap show match_list
    cstr = concat $ intersperse "\n" cstrlist
    cdot = if (length . psMatchs $ ps) > maxmatchs then "...\n" else ""
    region_list = psRegions ps
    rstrlist = fmap show (take maxregions $ miRegions region_list)
    rstr = concat $ intersperse "\n" rstrlist
    rdot = if (length . miRegions $ region_list) > maxregions then "...\n" else ""

matchCount :: PeepState -> Int
matchCount = length . psMatchs

regionCount :: PeepState -> Int
regionCount = length . miRegions . psRegions
