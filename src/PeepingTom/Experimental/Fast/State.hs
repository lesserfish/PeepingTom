{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PeepingTom.Experimental.Fast.State where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Foldable as F
import Data.IORef
import Data.Sequence (Seq, empty, (|>))
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import GHC.ForeignPtr
import qualified PeepingTom.IO as IO
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.State as Default
import qualified PeepingTom.Type as T
import Text.Printf

type MSeq a = IORef (Seq a)

makeMSeq :: IO (MSeq a)
makeMSeq = newIORef empty

push :: MSeq a -> a -> IO ()
push seqRef element = do
    mseq <- readIORef seqRef
    writeIORef seqRef (mseq |> element)

toList :: MSeq a -> IO [a]
toList seqRef = do
    mseq <- readIORef seqRef
    return $ F.toList mseq

data CFilter = CFilter {cfTypes :: [T.Type]}

-- This whole thing is dumb because we are
-- 1. Loading the data using FFI
-- 2. Converting it to ByteString
-- 3. Converting the bytestring to a Ptr
-- 4. Scanning using FFI
--
-- We might as well just use C

maxSizeOf :: [T.Type] -> Size
maxSizeOf types = output
  where
    output = foldr max 0 (map T.sizeOf types)

foreign import capi safe "ChunkReader.c scan"
    c_scan :: StablePtr (MSeq Int) -> Ptr CChar -> Int -> Int -> FunPtr (Ptr CChar -> Int -> Bool) -> Int -> IO ()

foreign import capi safe "ChunkReader.c &int32_eq"
    c_int32eq :: FunPtr (Ptr CChar -> Int -> Bool)

appendAddr :: StablePtr (MSeq Address) -> Int -> IO ()
appendAddr refptr value = do
    ref <- deRefStablePtr refptr
    push ref (fromIntegral value)

foreign export capi appendAddr :: StablePtr (MSeq Int) -> Int -> IO ()

regionScanHelper :: IO.RInterface -> CFilter -> Size -> (Address, Address) -> Default.ScanOptions -> IO [Default.Candidate]
regionScanHelper rinterface fltr regid (start_address, end_address) scopt = do
    if start_address >= end_address
        then return []
        else do
            let chunk_size = Default.soChunkSize scopt
            let types = cfTypes fltr
            let max_size = maxSizeOf types
            let offset_size = min (end_address - start_address) chunk_size
            let read_size = min (end_address - start_address) (chunk_size + max_size)
            let offset = [0 .. offset_size]
            tail <- regionScanHelper rinterface fltr regid (start_address + offset_size + 1, end_address) scopt
            chunk <- rinterface start_address read_size
            if IO.mcOk chunk
                then do
                    addrSeq <- makeMSeq :: IO (MSeq Address)
                    addrSeqPtr <- newStablePtr addrSeq
                    let (bsFptr, bsSize) = BSI.toForeignPtr0 . IO.mcData $ chunk
                    let bsPtr = unsafeForeignPtrToPtr bsFptr
                    c_scan addrSeqPtr (castPtr bsPtr) 0 read_size c_int32eq 42
                    addrlist <- toList addrSeq
                    let candidates = map (\addr -> Default.Candidate addr BS.empty types regid) addrlist
                    return $ candidates ++ tail
                else do
                    putStrLn $ printf "Failed to load chunk (%8x, %8x) of region %d" start_address (start_address + chunk_size) regid
                    return tail

regionScan :: IO.RInterface -> CFilter -> Maps.Region -> Default.ScanOptions -> IO [Default.Candidate]
regionScan rinterface fltr region scopt = do
    let rid = Maps.rID region
    let sa = Maps.rStartAddr region
    let ea = Maps.rEndAddr region
    candidates <- regionScanHelper rinterface fltr rid (sa, ea) scopt
    return candidates

regionScanLog :: Maps.Region -> [Default.Candidate] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d candidates from Region %4d (size = %8x)" (length result) (Maps.rID reg) ((Maps.rEndAddr reg) - (Maps.rStartAddr reg))

vocal :: (a -> b -> IO ()) -> (a -> IO b) -> a -> (IO b)
vocal log action input = do
    output <- action input
    log input output
    return output

scanMapHelper :: Default.ScanOptions -> CFilter -> Maps.MapInfo -> IO [Default.Candidate]
scanMapHelper scopt fltr map = do
    let stopsig = Default.soSIGSTOP scopt
    let pid = Maps.miPID map
    let regions = Maps.miRegions map
    candidates <-
        IO.withRInterface
            pid
            stopsig
            ( \rinterface -> do
                let action = vocal regionScanLog (\x -> regionScan rinterface fltr x scopt) :: Maps.Region -> IO [Default.Candidate]
                fc <- forM regions action :: IO [[Default.Candidate]]
                return $ concat fc
            )
    return $ candidates

scanMapS :: Default.ScanOptions -> CFilter -> Maps.MapInfo -> IO Default.PeepState
scanMapS scopt fltr map = do
    let pid = (Maps.miPID map)
    candidates <- scanMapHelper scopt fltr map
    return Default.PeepState{Default.psPID = pid, Default.psCandidates = candidates, Default.psRegions = map}

scanMap :: CFilter -> Maps.MapInfo -> IO Default.PeepState
scanMap = scanMapS Default.defaultScanOptions
