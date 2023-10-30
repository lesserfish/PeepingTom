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
        StablePtr (MSeq a) -> -- Pointer to table of accepted matchs
        CUIntPtr -> -- Starting address of Memory chunk
        CUIntPtr -> -- Size of Memory Chunk
        Ptr CChar -> -- Pointer to Memory chunk data
        Ptr CChar -> -- Pointer to reference value
        CSize -> -- sizeof reference value
        IO ()

appendMatch :: StablePtr (MSeq Match) -> CUIntPtr -> CUInt -> Ptr CChar -> CSize -> IO ()
appendMatch tblPtr cAddr cMatch mDataPtr mDataSize = do
    let types = decodeTypes cMatch (fromIntegral mDataSize)
    let cslenData = (mDataPtr, fromIntegral mDataSize) :: CStringLen
    bsData <- BS.packCStringLen cslenData
    let match =
            Match
                { mAddress = fromIntegral cAddr
                , mData = bsData
                , mTypes = types
                , mRegionID = 0
                }

    tbl <- deRefStablePtr tblPtr
    push tbl match
    return ()

foreign export capi appendMatch :: StablePtr (MSeq Match) -> CUIntPtr -> CUInt -> Ptr CChar -> CSize -> IO ()

regionScanHelper ::
    StablePtr (MSeq Match) -> -- Pointer to Match Table
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

regionScan :: IO.RInterface -> CFilter -> Region -> Size -> IO [Match]
regionScan rInterface cFltr region chunkSize = do
    matchSeq <- makeMSeq :: IO (MSeq Match)
    matchSeqPtr <- newStablePtr matchSeq
    let sa = rStartAddr region
    let ea = rEndAddr region
    let refPtr = getBSPtr (cfReference cFltr)
    let regID = rID region

    regionScanHelper matchSeqPtr refPtr rInterface cFltr (sa, ea) chunkSize
    matches' <- toList matchSeq
    let matchs = map (\match -> match{mRegionID = regID}) matches'
    return matchs

regionScanLog :: Region -> [Match] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d matchs from Region %4d (size = %8x)" (length result) (rID reg) ((rEndAddr reg) - (rStartAddr reg))

scanMapHelper :: SlowScan.ScanOptions -> CFilter -> MapInfo -> IO [Match]
scanMapHelper scopt cFltr map = do
    let chunkSize = SlowScan.soChunkSize scopt
    let stopsig = SlowScan.soSIGSTOP scopt
    let pid = miPID map
    let regions = miRegions map
    matchs <-
        IO.withRInterface
            pid
            stopsig
            ( \rinterface -> do
                let action = vocal regionScanLog (\x -> regionScan rinterface cFltr x chunkSize) :: Region -> IO [Match]
                fc <- forM regions action :: IO [[Match]]
                return $ concat fc
            )
    return $ matchs

-- Public Methods:

scanMapS :: SlowScan.ScanOptions -> CFilter -> MapInfo -> IO PeepState
scanMapS scopt fltr map = do
    let pid = (miPID map)
    matchs <- scanMapHelper scopt fltr map
    return PeepState{psPID = pid, psMatches = matchs, psRegions = map}

scanMap :: CFilter -> MapInfo -> IO PeepState
scanMap = scanMapS SlowScan.defaultScanOptions
