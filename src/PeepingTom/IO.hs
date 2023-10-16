{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PeepingTom.IO where

import Control.DeepSeq
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import GHC.Generics
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.Posix as Posix
import Text.Printf

data RegionData = RegionData {rData :: BS.ByteString, rInfo :: Maps.Region}
data MemoryChunk = MemoryChunk {startAddress :: Address, chunkSize :: Size, mData :: BS.ByteString}

instance Show RegionData where
    show rdata =
        show (rInfo rdata)
            ++ " \tBytes Read: "
            ++ show (BS.length (rData rdata))

memFile :: PID -> String
memFile pid = printf "/proc/%d/mem" pid

-- Open / Close

openFile :: FilePath -> Posix.ACCESS_MODE -> IO FD
openFile fp mode = do
    fd <- Posix.open fp mode
    return fd

closeFile :: FD -> IO ()
closeFile fd = do
    _ <- Posix.close fd
    return ()

-- Attaching implies
-- 1. Sending a SIGSTOP to the process
-- 2. Opening a file descriptor to /proc/pid/mem
attach :: PID -> Posix.ACCESS_MODE -> IO FD
attach pid mode = do
    _ <- Posix.kill pid Posix.SIGSTOP
    openFile (memFile pid) mode

-- Dettaching implies
-- 1. Sending a SIGCONT to the process
-- 2. Closing the file descriptor associated to /proc/pid/mem
dettach :: (PID, FD) -> IO ()
dettach (pid, fd) = do
    closeFile fd
    _ <- Posix.kill pid Posix.SIGCONT
    return ()

-- Chunks

readFileSectionByChunks :: FD -> (Address, Address) -> Size -> BS.ByteString -> IO BS.ByteString
readFileSectionByChunks fd (start_address, end_address) chunk_size bs
    | start_address >= end_address = return bs
    | otherwise = do
        let count = min (fromIntegral (end_address - start_address)) chunk_size
        chunk <- Posix.pread fd count start_address
        let bs' = BS.append bs chunk
        readFileSectionByChunks fd (start_address + (fromIntegral chunk_size), end_address) chunk_size bs'

readFileSection :: FD -> (Address, Address) -> Size -> IO BS.ByteString
readFileSection fd (start_address, end_address) chunk_size = do
    bytes <- readFileSectionByChunks fd (start_address, end_address) chunk_size BS.empty
    return bytes

loadRegion :: FD -> Size -> Maps.Region -> IO RegionData
loadRegion fd chunk_size region = do
    let start_address = Maps.startAddress region
    let end_address = Maps.endAddress region
    bytes <- readFileSection fd (start_address, end_address) chunk_size
    let rdata = RegionData bytes region :: RegionData
    return rdata

cLoadMap :: Size -> Maps.MapInfo -> IO [RegionData]
cLoadMap chunk_size m = do
    let pid = Maps.mPID m
    fd <- attach pid Posix.O_RDONLY
    output <- sequence $ fmap (loadRegion fd chunk_size) (Maps.regions m)
    dettach (pid, fd)
    return output

defaultChunkSize :: Size
defaultChunkSize = 16384

loadMap :: Maps.MapInfo -> IO [RegionData]
loadMap mapinfo = cLoadMap defaultChunkSize mapinfo

loadMemoryChunk :: FD -> Address -> Size -> IO MemoryChunk
loadMemoryChunk fd addr chunk_size = do
    chunk <- Posix.pread fd chunk_size addr
    return $ MemoryChunk addr chunk_size chunk

-- Read

{-
updateCandidate :: [Candidate] -> MemoryChunk -> IO [Candidate]
updateCandidate [] = []
updateCandidate (current : rest) memory_chunk = do
    tail <- updateCandidate rest memory_chunk
    if current_inside memory_chunk
        then do
            read from chunk
            update candidate
            return [new_candidate] ++ tail
        else do
            new_chunk <- from_candidate_addr
            return updateCandidate (current : rest) new_chunk
-}
