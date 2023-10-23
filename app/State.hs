module State where

import qualified Data.Map as Map
import qualified PeepingTom.State as PT
import qualified PeepingTom.Type as PTType
import Text.Printf

type PTMap = Map.Map String PT.PeepState

data RFilter = RFDefault deriving (Show)
data Options = Options
    { oScanTypes :: [PTType.Type]
    , oRFilter :: RFilter
    , oChunkSize :: Int
    }

data State = State
    { sStates :: PTMap
    , sCurrentState :: String
    , sPID :: Int
    , sOptions :: Options
    }

defaultOptions :: Options
defaultOptions =
    Options
        { oScanTypes = PTType.intTypes
        , oRFilter = RFDefault
        , oChunkSize = 10000
        }

instance Show Options where
    show (Options types fltr chunk_size) = printf "Types: %s\nFilter: %s\nChunk size: %s bytes\n" (show types) (show fltr) (show chunk_size)

emptyState :: State
emptyState =
    State
        { sStates = Map.fromList []
        , sCurrentState = "default"
        , sPID = 0
        , sOptions = defaultOptions
        }