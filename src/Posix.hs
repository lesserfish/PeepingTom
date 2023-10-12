{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- This is a barebones implementation of the Posix syscalls that I need to able to read/modify virtual memory of other processes
-- This is NOT a comprehensive implementation of ANY C library or API.
-- I have implemented only what I need for PeepingTom to work.
-- These functions also throw HARD errors on failure, so they are NOT suitable for usage in anything more complex than PeepingTom.
-- (In fact, I should probably avoid these hard errors)

module Posix where

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Foreign.C
import Foreign.Ptr
import Text.Printf

type FD = Int

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

pwrite :: FD -> B.ByteString -> Int -> Int -> IO Int
pwrite fd raw_data count offset = do
    let pwrite' buffer = do
            let ciFD = fromIntegral fd :: CInt
            let csBytes = fromIntegral count :: CSize
            let csOffset = fromIntegral offset :: CSize
            csBytes_written <- raw_pwrite ciFD buffer csBytes csOffset
            let iBytes_written = fromIntegral csBytes_written
            if iBytes_written < 0 then (throw $ PosixException (printf "Could not write to file with pid %d." fd)) else return ()
            return iBytes_written
    iBytes_written <- BU.unsafeUseAsCString raw_data pwrite'
    return iBytes_written

-- pread
foreign import capi safe "unistd.h pread"
    raw_pread :: CInt -> Ptr a -> CSize -> CSize -> IO CSize

pread :: FD -> Int -> Int -> IO B.ByteString
pread fd count offset = do
    let pread' (buffer, iBufsize) = do
            let csBufsize = fromIntegral iBufsize :: CSize
            let ciFD = fromIntegral fd :: CInt
            let csOffset = fromIntegral offset :: CSize
            ciBytes_read <- raw_pread ciFD buffer csBufsize csOffset
            let iBytes_read = fromIntegral ciBytes_read :: Int
            if (iBytes_read) < 0 then (throw $ PosixException (printf "Could not read file with PID %d. Are you sure you have the permission?" fd)) else return ()
            bytestring <- BU.unsafePackCStringLen (buffer, iBytes_read) :: IO B.ByteString
            return bytestring
    bytestring <- withCStringLen (replicate count '\0') pread'
    return bytestring

-- MAX_PATH
foreign import capi safe "helper.h get_path_max"
    raw_maxpath :: CInt

maxPath :: Int
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
    rl <- withCString filepath (\fp -> withCStringLen (replicate maxPath '\0') (\(buf, bsize) -> readlink' fp (buf, bsize)))
    return rl

-- ptrace
data PTRACE_REQUEST
    = PTRACE_TRACEME
    | PTRACE_PEEKTEXT
    | PTRACE_PEEKDATA
    | PTRACE_PEEKUSER
    | PTRACE_POKETEXT
    | PTRACE_POKEDATA
    | PTRACE_POKEUSER
    | PTRACE_CONT
    | PTRACE_KILL
    | PTRACE_SINGLESTEP
    | PTRACE_GETREGS
    | PTRACE_SETREGS
    | PTRACE_GETFPREGS
    | PTRACE_SETFPREGS
    | PTRACE_ATTACH
    | PTRACE_DETACH
    | PTRACE_GETFPXREGS
    | PTRACE_SETFPXREGS
    | PTRACE_SYSCALL
    | PTRACE_SETOPTIONS
    | PTRACE_GETEVENTMSG
    | PTRACE_GETSIGINFO
    | PTRACE_SETSIGINFO

ptrace_request :: PTRACE_REQUEST -> Int
ptrace_request PTRACE_TRACEME = 0
ptrace_request PTRACE_PEEKTEXT = 1
ptrace_request PTRACE_PEEKDATA = 2
ptrace_request PTRACE_PEEKUSER = 3
ptrace_request PTRACE_POKETEXT = 4
ptrace_request PTRACE_POKEDATA = 5
ptrace_request PTRACE_POKEUSER = 6
ptrace_request PTRACE_CONT = 7
ptrace_request PTRACE_KILL = 8
ptrace_request PTRACE_SINGLESTEP = 9
ptrace_request PTRACE_GETREGS = 12
ptrace_request PTRACE_SETREGS = 13
ptrace_request PTRACE_GETFPREGS = 14
ptrace_request PTRACE_SETFPREGS = 15
ptrace_request PTRACE_ATTACH = 16
ptrace_request PTRACE_DETACH = 17
ptrace_request PTRACE_GETFPXREGS = 18
ptrace_request PTRACE_SETFPXREGS = 19
ptrace_request PTRACE_SYSCALL = 24
ptrace_request PTRACE_SETOPTIONS = 0x4200
ptrace_request PTRACE_GETEVENTMSG = 0x4201
ptrace_request PTRACE_GETSIGINFO = 0x4202
ptrace_request PTRACE_SETSIGINFO = 0x4203

foreign import capi safe "sys/ptrace.h ptrace"
    raw_ptrace :: CInt -> CInt -> Ptr CChar -> Ptr CChar -> IO CLong
