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
    , oSendStopSig :: Bool
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
        , oSendStopSig = True
        }

instance Show Options where
    show (Options types fltr chunk_size stop_sig) = printf "Types: %s\nFilter: %s\nChunk size: %s bytes\nSend Stop Signal: %s" (show types) (show fltr) (show chunk_size) (show stop_sig)

emptyState :: State
emptyState =
    State
        { sStates = Map.fromList []
        , sCurrentState = "default"
        , sPID = 0
        , sOptions = defaultOptions
        }
