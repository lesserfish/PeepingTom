{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PeepingTom.IO where

import Control.DeepSeq
import qualified Data.ByteString as BS
import GHC.Generics
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.Posix as Posix
import Text.Printf

data RegionData = RegionData {rData :: BS.ByteString, rInfo :: Maps.Region}

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
    --    _ <- Posix.kill pid Posix.SIGSTOP
    openFile (memFile pid) mode

-- Dettaching implies
-- 1. Sending a SIGCONT to the process
-- 2. Closing the file descriptor associated to /proc/pid/mem
dettach :: (PID, FD) -> IO ()
dettach (pid, fd) = do
    closeFile fd
    --    _ <- Posix.kill pid Posix.SIGCONT
    return ()

-- Chunks

readChunks :: FD -> (Address, Address) -> Size -> BS.ByteString -> IO BS.ByteString
readChunks fd (start_address, end_address) chunk_size bs
    | start_address >= end_address = return bs
    | otherwise = do
        let count = min (fromIntegral (end_address - start_address)) chunk_size
        chunk <- Posix.pread fd count start_address
        let bs' = BS.append bs chunk
        readChunks fd (start_address + (fromIntegral chunk_size), end_address) chunk_size bs'

readFileSection :: FD -> (Address, Address) -> Size -> IO BS.ByteString
readFileSection fd (start_address, end_address) chunk_size = do
    bytes <- readChunks fd (start_address, end_address) chunk_size BS.empty
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
defaultChunkSize = 4096

loadMap :: Maps.MapInfo -> IO [RegionData]
loadMap mapinfo = cLoadMap defaultChunkSize mapinfo

-- Bytes

-- Read Interface : Address -> Byte_Count -> IO ByteAddress
type RInterface = Address -> Size -> IO BS.ByteString

-- Write Interface : Address -> ByteString -> IO ()
type WInterface = Address -> BS.ByteString -> IO ()

readBytes :: FD -> Address -> Size -> IO BS.ByteString
readBytes fd addr byte_count = do
    Posix.pread fd byte_count addr

writeBytes :: FD -> Address -> BS.ByteString -> IO ()
writeBytes fd addr bytes = do
    _ <- Posix.pwrite fd bytes addr
    return ()

withReadInterface :: PID -> (RInterface -> IO a) -> IO a
withReadInterface pid func = do
    fd <- attach pid Posix.O_RDONLY
    let rinterface = readBytes fd :: RInterface
    output <- func rinterface
    dettach (pid, fd)
    return output

withWriteInterface :: PID -> (WInterface -> IO a) -> IO a
withWriteInterface pid func = do
    fd <- attach pid Posix.O_WRONLY
    let winterface = writeBytes fd :: WInterface
    output <- func winterface
    dettach (pid, fd)
    return output
