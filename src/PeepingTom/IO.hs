module PeepingTom.IO (
    MemoryChunk (..),
    RInterface,
    WInterface,
    withWInterface,
    withRInterface,
) where

import qualified Data.ByteString as BS
import PeepingTom.Internal
import qualified PeepingTom.Posix as Posix
import Text.Printf (printf)

data MemoryChunk = MemoryChunk {mcOk :: Bool, mcStartAddr :: Address, mcSize :: Size, mcData :: BS.ByteString}
type WInterface = (Address -> BS.ByteString -> IO ())
type RInterface = Address -> Size -> IO MemoryChunk

memFile :: PID -> String
memFile pid = printf "/proc/%d/mem" pid

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
attach :: PID -> Posix.ACCESS_MODE -> Bool -> IO FD
attach pid mode stopsig = do
    _ <- if stopsig then Posix.kill pid Posix.SIGSTOP else return 0
    openFile (memFile pid) mode

-- Dettaching implies
-- 1. Sending a SIGCONT to the process
-- 2. Closing the file descriptor associated to /proc/pid/mem
dettach :: (PID, FD) -> Bool -> IO ()
dettach (pid, fd) contsig = do
    closeFile fd
    _ <- if contsig then Posix.kill pid Posix.SIGCONT else return 0
    return ()

loadMemoryChunk :: FD -> Address -> Size -> IO MemoryChunk
loadMemoryChunk fd addr chunk_size = do
    (chunk, bytes_read) <- Posix.pread fd chunk_size addr
    return $ MemoryChunk{mcOk = (bytes_read >= 0), mcData = chunk, mcSize = chunk_size, mcStartAddr = addr}

-- Read / Write

writeInterface :: FD -> WInterface
writeInterface fd addr bs = do
    if BS.length bs > 0
        then do
            _ <- Posix.pwrite fd bs addr -- TODO: Handle errors in here maybe?
            return ()
        else return ()

withWInterface :: PID -> Bool -> (WInterface -> IO a) -> IO a
withWInterface pid stopsig action = do
    fd <- attach pid Posix.O_WRONLY stopsig
    let winterface = writeInterface fd
    output <- action winterface
    dettach (pid, fd) stopsig
    return output

readInterface :: FD -> RInterface
readInterface fd addr size = loadMemoryChunk fd addr size

withRInterface :: PID -> Bool -> (RInterface -> IO a) -> IO a
withRInterface pid stopsig action = do
    fd <- attach pid Posix.O_RDONLY stopsig
    let cinterface = readInterface fd
    output <- action cinterface
    dettach (pid, fd) stopsig
    return output
