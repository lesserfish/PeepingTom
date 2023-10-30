{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PeepingTom.Fast.Scan (
    scanMapS,
    scanMap,
    SlowScan.defaultScanOptions,
    SlowScan.updateState,
    SlowScan.updateStateS,
    SlowScan.ScanOptions (..),
) where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import GHC.ForeignPtr
import PeepingTom.Common
import PeepingTom.Fast.Common
import PeepingTom.Fast.Filter
import PeepingTom.Fast.MSeq
import qualified PeepingTom.IO as IO
import PeepingTom.Internal
import PeepingTom.Map
import qualified PeepingTom.Scan as SlowScan
import PeepingTom.State
import PeepingTom.Type
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

appendMatch :: StablePtr (MSeq Candidate) -> CUIntPtr -> CUInt -> Ptr CChar -> CSize -> IO ()
appendMatch tblPtr cAddr cMatch cDataPtr cDataSize = do
    let types = decodeTypes cMatch (fromIntegral cDataSize)
    let cslenData = (cDataPtr, fromIntegral cDataSize) :: CStringLen
    bsData <- BS.packCStringLen cslenData
    let match =
            Candidate
                { cAddress = fromIntegral cAddr
                , cData = bsData
                , cTypes = types
                , cRegionID = 0
                }

    tbl <- deRefStablePtr tblPtr
    push tbl match
    return ()

foreign export capi appendMatch :: StablePtr (MSeq Candidate) -> CUIntPtr -> CUInt -> Ptr CChar -> CSize -> IO ()

regionScanHelper ::
    StablePtr (MSeq Candidate) -> -- Pointer to Candidate Table
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

regionScan :: IO.RInterface -> CFilter -> Region -> Size -> IO [Candidate]
regionScan rInterface cFltr region chunkSize = do
    matchSeq <- makeMSeq :: IO (MSeq Candidate)
    matchSeqPtr <- newStablePtr matchSeq
    let sa = rStartAddr region
    let ea = rEndAddr region
    let refPtr = getBSPtr (cfReference cFltr)
    let regID = rID region

    regionScanHelper matchSeqPtr refPtr rInterface cFltr (sa, ea) chunkSize
    matches' <- toList matchSeq
    let candidates = map (\candidate -> candidate{cRegionID = regID}) matches'
    return candidates

regionScanLog :: Region -> [Candidate] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d candidates from Region %4d (size = %8x)" (length result) (rID reg) ((rEndAddr reg) - (rStartAddr reg))

scanMapHelper :: SlowScan.ScanOptions -> CFilter -> MapInfo -> IO [Candidate]
scanMapHelper scopt cFltr map = do
    let chunkSize = SlowScan.soChunkSize scopt
    let stopsig = SlowScan.soSIGSTOP scopt
    let pid = miPID map
    let regions = miRegions map
    candidates <-
        IO.withRInterface
            pid
            stopsig
            ( \rinterface -> do
                let action = vocal regionScanLog (\x -> regionScan rinterface cFltr x chunkSize) :: Region -> IO [Candidate]
                fc <- forM regions action :: IO [[Candidate]]
                return $ concat fc
            )
    return $ candidates

-- Public Methods:

scanMapS :: SlowScan.ScanOptions -> CFilter -> MapInfo -> IO PeepState
scanMapS scopt fltr map = do
    let pid = (miPID map)
    candidates <- scanMapHelper scopt fltr map
    return PeepState{psPID = pid, psCandidates = candidates, psRegions = map}

scanMap :: CFilter -> MapInfo -> IO PeepState
scanMap = scanMapS SlowScan.defaultScanOptions
