module PeepingTom.IO where

import qualified Data.ByteString as BS
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.Posix as Posix
import Text.Printf (printf)

data MemoryChunk = MemoryChunk {mcStartAddr :: Address, mcSize :: Size, mcData :: BS.ByteString}
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

loadMemoryChunk :: FD -> Address -> Size -> IO MemoryChunk
loadMemoryChunk fd addr chunk_size = do
    chunk <- Posix.pread fd chunk_size addr
    return $ MemoryChunk addr chunk_size chunk

-- Read / Write

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
readInterface fd addr size = loadMemoryChunk fd addr size

withRInterface :: PID -> (RInterface -> IO a) -> IO a
withRInterface pid action = do
    fd <- attach pid Posix.O_RDONLY
    let cinterface = readInterface fd
    output <- action cinterface
    dettach (pid, fd)
    return output
