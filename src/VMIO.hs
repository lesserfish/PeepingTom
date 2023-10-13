module VMIO where

import qualified Data.ByteString as B
import Internal
import qualified Maps as M
import qualified Posix as P
import Text.Printf

data RData = RData {regdata :: B.ByteString, reginfo :: M.Region}

instance Show RData where
    show rdata =
        show (reginfo rdata)
            ++ " \tBytes Read: "
            ++ show (B.length (regdata rdata))

memFile :: PID -> String
memFile pid = printf "/proc/%d/mem" pid

-- Open / Close

openFile :: FilePath -> P.ACCESS_MODE -> IO FD
openFile fp mode = do
    fd <- P.open fp mode
    return fd

closeFile :: FD -> IO ()
closeFile fd = do
    _ <- P.close fd
    return ()

-- Attaching implies
-- 1. Sending a SIGSTOP to the process
-- 2. Opening a file descriptor to /proc/pid/mem
attach :: PID -> P.ACCESS_MODE -> IO FD
attach pid mode = do
    _ <- P.kill pid P.SIGSTOP
    openFile (memFile pid) mode

-- Dettaching implies
-- 1. Sending a SIGCONT to the process
-- 2. Closing the file descriptor associated to /proc/pid/mem
dettach :: (PID, FD) -> IO ()
dettach (pid, fd) = do
    closeFile fd
    _ <- P.kill pid P.SIGCONT
    return ()

-- Chunks

readChunks :: FD -> (Address, Address) -> Size -> B.ByteString -> IO B.ByteString
readChunks fd (start_address, end_address) chunk_size bs
    | start_address >= end_address = return bs
    | otherwise = do
        let count = min (fromIntegral (end_address - start_address)) chunk_size
        chunk <- P.pread fd count start_address
        let bs' = B.append bs chunk
        readChunks fd (start_address + (fromIntegral chunk_size), end_address) chunk_size bs'

readFileSection :: FD -> (Address, Address) -> Size -> IO B.ByteString
readFileSection fd (start_address, end_address) chunk_size = do
    bytes <- readChunks fd (start_address, end_address) chunk_size B.empty
    return bytes

loadRegion :: FD -> Size -> M.Region -> IO RData
loadRegion fd chunk_size region = do
    let start_address = M.startAddress region
    let end_address = M.endAddress region
    bytes <- readFileSection fd (start_address, end_address) chunk_size
    let rdata = RData bytes region :: RData
    putStrLn $ "Finished loading region " ++ show (M.regionID region)
    return rdata

cLoadMap :: Size -> M.MapInfo -> IO [RData]
cLoadMap chunk_size m = do
    let pid = M.mPID m
    fd <- attach pid P.O_RDONLY
    output <- sequence $ fmap (loadRegion fd chunk_size) (M.regions m)
    dettach (pid, fd)
    return output

defaultChunkSize :: Size
defaultChunkSize = 4096

loadMap :: M.MapInfo -> IO [RData]
loadMap mapinfo = cLoadMap defaultChunkSize mapinfo

-- Bytes

-- Read Interface : Address -> Byte_Count -> IO ByteAddress
type RInterface = Address -> Size -> IO B.ByteString

-- Write Interface : Address -> ByteString -> IO ()
type WInterface = Address -> B.ByteString -> IO ()

readBytes :: FD -> Address -> Size -> IO B.ByteString
readBytes fd addr byte_count = do
    P.pread fd byte_count addr

writeBytes :: FD -> Address -> B.ByteString -> IO ()
writeBytes fd addr bytes = do
    _ <- P.pwrite fd bytes addr
    return ()

withReadInterface :: PID -> (RInterface -> IO a) -> IO a
withReadInterface pid func = do
    fd <- attach pid P.O_RDONLY
    let rinterface = readBytes fd :: RInterface
    output <- func rinterface
    dettach (pid, fd)
    return output

withWriteInterface :: PID -> (WInterface -> IO a) -> IO a
withWriteInterface pid func = do
    fd <- attach pid P.O_WRONLY
    let winterface = writeBytes fd :: WInterface
    output <- func winterface
    dettach (pid, fd)
    return output
