{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PeepingTom.IO where

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

-- Read / Write

type WInterface = (Address -> BS.ByteString -> IO ())
type RInterface = (Address -> Size -> IO BS.ByteString)

writeInterface :: FD -> WInterface
writeInterface fd addr bs = do
    _ <- Posix.pwrite fd bs addr -- TODO: Handle errors in here maybe?
    return ()

withWInterface :: PID -> (WInterface -> IO a) -> IO a
withWInterface pid action = do
    fd <- attach pid Posix.O_WRONLY
    let winterface = writeInterface fd
    output <- action winterface
    dettach (pid, fd)
    return output

readInterface :: FD -> RInterface
readInterface fd addr size = do
    bs <- Posix.pread fd size addr
    return bs

withRInterface :: PID -> (RInterface -> IO a) -> IO a
withRInterface pid action = do
    fd <- attach pid Posix.O_RDONLY
    let rinterface = readInterface fd
    output <- action rinterface
    dettach (pid, fd)
    return output

type CInterface = Address -> Size -> IO MemoryChunk

chunkInterface :: FD -> CInterface
chunkInterface fd addr size = loadMemoryChunk fd addr size

withCInterface :: PID -> (CInterface -> IO a) -> IO a
withCInterface pid action = do
    fd <- attach pid Posix.O_RDONLY
    let cinterface = chunkInterface fd
    output <- action cinterface
    dettach (pid, fd)
    return output
