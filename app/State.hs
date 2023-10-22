module State where

import qualified Data.Map as Map
import qualified PeepingTom.State as PT
import qualified PeepingTom.Type as PTType

type PTMap = Map.Map String PT.PeepState

data RFilter = RFDefault
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
        { oScanTypes = [PTType.Int32]
        , oRFilter = RFDefault
        , oChunkSize = 10000
        }

emptyState :: State
emptyState =
    State
        { sStates = Map.fromList []
        , sCurrentState = "default"
        , sPID = 0
        , sOptions = defaultOptions
        }
