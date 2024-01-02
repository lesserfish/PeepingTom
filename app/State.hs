module State where

import qualified Data.Map as Map
import qualified PeepingTom.State as PT
import qualified PeepingTom.Type as PTType
import Text.Printf

type PTMap = Map.Map String PT.PeepState

data RFilter
    = RFDefault
    | RFReadPerm
    | RFWritePerm
    | RFAll
    deriving (Show)

data ScanTypes = Int8 | Int16 | Int32 | Int64 | Int | Str deriving (Show)
data Options = Options
    { oScanTypes :: ScanTypes
    , oRFilter :: RFilter
    , oChunkSize :: Int
    , oSendStopSig :: Bool
    , oFastMode :: Bool
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
        { oScanTypes = Int
        , oRFilter = RFDefault
        , oChunkSize = 10000
        , oSendStopSig = True
        , oFastMode = True
        }

instance Show Options where
    show (Options types fltr chunk_size stop_sig fm) = printf "Types: %s\nFilter: %s\nChunk size: %s bytes\nSend Stop Signal: %s\nFast Mode: %s" (show types) (show fltr) (show chunk_size) (show stop_sig) (show fm)

emptyState :: State
emptyState =
    State
        { sStates = Map.fromList []
        , sCurrentState = "default"
        , sPID = 0
        , sOptions = defaultOptions
        }
