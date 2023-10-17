{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- This is a barebones implementation of the Posix syscalls that I need to able to read/modify virtual memory of other processes
-- This is NOT a comprehensive implementation of ANY C library or API.
-- I have implemented only what I need for PeepingTom to work.
-- These functions also throw HARD errors on failure, so they are NOT suitable for usage in anything more complex than PeepingTom.
-- (In fact, I should probably avoid these hard errors)

module PeepingTom.Posix where

import Control.Exception
import qualified Data.ByteString as BS
import Foreign.C
import Foreign.Ptr
import PeepingTom.Internal
import System.Posix.Types
import Text.Printf (printf)

data PosixException = PosixException {exception :: String}
instance Exception PosixException
instance Show PosixException where
    show = exception

data ACCESS_MODE = O_RDONLY | O_WRONLY | O_RDWR
access_mode :: ACCESS_MODE -> CInt
access_mode O_RDONLY = 0
access_mode O_WRONLY = 1
access_mode O_RDWR = 2

-- open
foreign import capi safe "fcntl.h open"
    raw_open :: Ptr CChar -> CInt -> IO CInt

open :: String -> ACCESS_MODE -> IO FD
open fp mode = do
    let imode = fromIntegral $ access_mode mode :: CInt
    fd <- withCString fp (\x -> raw_open x imode) :: IO CInt
    if (fromIntegral fd :: Int) < 0 then (throw $ PosixException (printf "Could not read file '%s'. Are you sure you have the permissions?" fp)) else return ()
    return (fromIntegral fd)

-- close
foreign import capi safe "unistd.h close"
    raw_close :: CInt -> IO CInt

close :: FD -> IO Int
close fd = do
    status <- raw_close (fromIntegral fd)
    if (fromIntegral status :: Int) < 0 then (throw $ PosixException (printf "Could not close file with pid %d." fd)) else return ()
    return (fromIntegral status)

-- pwrite
foreign import capi safe "unistd.h pwrite"
    raw_pwrite :: CInt -> Ptr a -> CSize -> CSize -> IO CSize

pwrite :: FD -> BS.ByteString -> Address -> IO Int
pwrite fd raw_data offset = do
    let count = BS.length raw_data
    let pwrite' buffer = do
            let ciFD = fromIntegral fd :: CInt
            let csBytes = fromIntegral count :: CSize
            let csOffset = fromIntegral offset :: CSize
            csBytes_written <- raw_pwrite ciFD buffer csBytes csOffset
            let iBytes_written = fromIntegral csBytes_written
            if iBytes_written < 0 then (throw $ PosixException (printf "Could not write to file with pid %d." fd)) else return ()
            return iBytes_written
    iBytes_written <- BS.useAsCString raw_data pwrite'
    return iBytes_written

-- pread
foreign import capi safe "unistd.h pread"
    raw_pread :: CInt -> Ptr a -> CSize -> CSize -> IO CSize

pread :: FD -> Size -> Address -> IO BS.ByteString
pread fd count offset = do
    let pread' (buffer, iBufsize) = do
            let csBufsize = fromIntegral iBufsize :: CSize
            let ciFD = fromIntegral fd :: CInt
            let csOffset = fromIntegral offset :: CSize
            ciBytes_read <- raw_pread ciFD buffer csBufsize csOffset
            let iBytes_read = fromIntegral ciBytes_read :: Int
            if (iBytes_read) < 0 then (throw $ PosixException (printf "Could not read file with PID %d. Are you sure you have the permission?" fd)) else return ()
            bytestring <- BS.packCStringLen (buffer, iBytes_read) :: IO BS.ByteString
            return bytestring
    bytestring <- withCStringLen (replicate (fromIntegral count) '\0') pread'
    evaluate bytestring
    return bytestring

-- MAX_PATH
foreign import capi safe "helper.h get_path_max"
    raw_maxpath :: CInt

maxPath :: Size
maxPath = fromIntegral raw_maxpath

-- readlink
foreign import capi safe "unistd.h readlink"
    raw_readlink :: Ptr CChar -> Ptr CChar -> CSize -> IO CSize

readlink :: String -> IO String
readlink filepath = do
    let readlink' pathname (buffer, iBufsize) = do
            let csBufsize = fromIntegral iBufsize :: CSize
            csBytes_read <- raw_readlink pathname buffer csBufsize
            let iBytes_read = fromIntegral csBytes_read :: Int
            if iBytes_read < 0 then (throw $ PosixException (printf "Could not readlink file %s." filepath)) else return ()
            output <- peekCStringLen (buffer, iBytes_read) :: IO String
            return output
    rl <- withCString filepath (\fp -> withCStringLen (replicate (fromIntegral maxPath) '\0') (\(buf, bsize) -> readlink' fp (buf, bsize)))
    return rl

-- kill

data SIGNAL
    = SIGHUP
    | SIGINT
    | SIGQUIT
    | SIGILL
    | SIGTRAP
    | SIGABRT
    | SIGBUS
    | SIGFPE
    | SIGKILL
    | SIGUSR1
    | SIGSEGV
    | SIGUSR2
    | SIGPIPE
    | SIGALRM
    | SIGTERM
    | SIGSTKFLT
    | SIGCHLD
    | SIGCONT
    | SIGSTOP
    | SIGTSTP
    | SIGTTIN
    | SIGTTOU
    | SIGURG
    | SIGXCPU
    | SIGXFSZ
    | SIGVTALRM
    | SIGPROF
    | SIGWINCH
    | SIGPOLL
    | SIGPWR
    | SIGSYS

signal :: SIGNAL -> Int
signal SIGHUP = 1
signal SIGINT = 2
signal SIGQUIT = 3
signal SIGILL = 4
signal SIGTRAP = 5
signal SIGABRT = 6
signal SIGBUS = 7
signal SIGFPE = 8
signal SIGKILL = 9
signal SIGUSR1 = 10
signal SIGSEGV = 11
signal SIGUSR2 = 12
signal SIGPIPE = 13
signal SIGALRM = 14
signal SIGTERM = 15
signal SIGSTKFLT = 16
signal SIGCHLD = 17
signal SIGCONT = 18
signal SIGSTOP = 19
signal SIGTSTP = 20
signal SIGTTIN = 21
signal SIGTTOU = 22
signal SIGURG = 23
signal SIGXCPU = 24
signal SIGXFSZ = 25
signal SIGVTALRM = 26
signal SIGPROF = 27
signal SIGWINCH = 28
signal SIGPOLL = 29
signal SIGPWR = 30
signal SIGSYS = 31

foreign import capi safe "signal.h kill"
    raw_kill :: CPid -> CInt -> IO CInt

kill :: PID -> SIGNAL -> IO Int
kill pid sig = do
    let cpid = fromIntegral pid :: CPid
    let csig = fromIntegral (signal sig) :: CInt
    cstatus <- raw_kill cpid csig
    let status = fromIntegral cstatus :: Int
    if status < 0 then (throw $ PosixException (printf "Could not send kill signal to PID %d." pid)) else return ()
    return status
