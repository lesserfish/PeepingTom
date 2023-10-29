{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PeepingTom.Experimental.Fast.State where

import Control.Monad (forM)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Maybe (catMaybes)
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import GHC.ForeignPtr
import PeepingTom.Experimental.Fast.Common
import PeepingTom.Experimental.Fast.Filters
import PeepingTom.Experimental.Fast.MSeq
import qualified PeepingTom.IO as IO
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.State as PT
import qualified PeepingTom.Type as T
import Text.Printf

foreign import capi safe "C/Scanner.c scan"
    c_scan ::
        FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt) -> -- Pointer to comparison function
        StablePtr (MSeq a) -> -- Pointer to table of accepted candidates
        CUIntPtr -> -- Starting address of Memory chunk
        CUIntPtr -> -- Size of Memory Chunk
        Ptr CChar -> -- Pointer to Memory chunk data
        Ptr CChar -> -- Pointer to reference value
        CSize -> -- sizeof reference value
        IO ()

appendMatch :: StablePtr (MSeq PT.Candidate) -> CUIntPtr -> CUInt -> Ptr CChar -> CSize -> IO ()
appendMatch tblPtr cAddr cMatch cDataPtr cDataSize = do
    let types = decodeTypes cMatch (fromIntegral cDataSize)
    let cslenData = (cDataPtr, fromIntegral cDataSize) :: CStringLen
    bsData <- BS.packCStringLen cslenData
    let match =
            PT.Candidate
                { PT.cAddress = fromIntegral cAddr
                , PT.cData = bsData
                , PT.cTypes = types
                , PT.cRegionID = 0
                }

    tbl <- deRefStablePtr tblPtr
    push tbl match
    return ()

foreign export capi appendMatch :: StablePtr (MSeq PT.Candidate) -> CUIntPtr -> CUInt -> Ptr CChar -> CSize -> IO ()

regionScanHelper ::
    StablePtr (MSeq PT.Candidate) -> -- Pointer to Candidate Table
    Ptr CChar -> -- Pointer to Reference bytestring
    IO.RInterface -> -- Read Interface
    CFilter -> -- The Filter in question
    (Address, Address) -> -- (Start_Address, End_Address) of chunk
    Size -> -- Chunk Size
    IO ()
--
regionScanHelper matchSeqPtr refPtr rInterface cFltr (startAddr, endAddr) chunkSize = do
    if startAddr >= endAddr
        then return ()
        else do
            let maxSize = cfMaxSize cFltr

            let offset_size = min (endAddr - startAddr) chunkSize
            let read_size = min (endAddr - startAddr) (chunkSize + maxSize)

            regionScanHelper matchSeqPtr refPtr rInterface cFltr (startAddr + offset_size + 1, endAddr) chunkSize

            chunk <- rInterface startAddr read_size
            if IO.mcOk chunk
                then do
                    let dataPtr = getBSPtr (IO.mcData chunk)
                    c_scan (cfFPtr cFltr) matchSeqPtr (fromIntegral startAddr) (fromIntegral offset_size) dataPtr refPtr (fromIntegral maxSize)
                    return ()
                else do
                    putStrLn $ printf "Failed to load chunk (%8x, %8x) of region." startAddr (startAddr + chunkSize)
                    return ()

regionScan :: IO.RInterface -> CFilter -> Maps.Region -> Size -> IO [PT.Candidate]
regionScan rInterface cFltr region chunkSize = do
    matchSeq <- makeMSeq :: IO (MSeq PT.Candidate)
    matchSeqPtr <- newStablePtr matchSeq
    let sa = Maps.rStartAddr region
    let ea = Maps.rEndAddr region
    let refPtr = getBSPtr (cfReference cFltr)
    let regID = Maps.rID region

    regionScanHelper matchSeqPtr refPtr rInterface cFltr (sa, ea) chunkSize
    matches' <- toList matchSeq
    let candidates = map (\candidate -> candidate{PT.cRegionID = regID}) matches'
    return candidates

regionScanLog :: Maps.Region -> [PT.Candidate] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d candidates from Region %4d (size = %8x)" (length result) (Maps.rID reg) ((Maps.rEndAddr reg) - (Maps.rStartAddr reg))

vocal :: (a -> b -> IO ()) -> (a -> IO b) -> a -> (IO b)
vocal log action input = do
    output <- action input
    log input output
    return output

scanMapHelper :: PT.ScanOptions -> CFilter -> Maps.MapInfo -> IO [PT.Candidate]
scanMapHelper scopt cFltr map = do
    let chunkSize = PT.soChunkSize scopt
    let stopsig = PT.soSIGSTOP scopt
    let pid = Maps.miPID map
    let regions = Maps.miRegions map
    candidates <-
        IO.withRInterface
            pid
            stopsig
            ( \rinterface -> do
                let action = vocal regionScanLog (\x -> regionScan rinterface cFltr x chunkSize) :: Maps.Region -> IO [PT.Candidate]
                fc <- forM regions action :: IO [[PT.Candidate]]
                return $ concat fc
            )
    return $ candidates

scanMapS :: PT.ScanOptions -> CFilter -> Maps.MapInfo -> IO PT.PeepState
scanMapS scopt fltr map = do
    let pid = (Maps.miPID map)
    candidates <- scanMapHelper scopt fltr map
    return PT.PeepState{PT.psPID = pid, PT.psCandidates = candidates, PT.psRegions = map}

scanMap :: CFilter -> Maps.MapInfo -> IO PT.PeepState
scanMap = scanMapS PT.defaultScanOptions

maxSizeOf :: [T.Type] -> Size
maxSizeOf types = output
  where
    output = foldr max 0 (map T.sizeOf types)
