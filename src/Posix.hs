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
data ACCESS_MODE = O_RDONLY | O_WRONLY | O_RDWR
data PosixException = PosixException {exception :: String}

instance Show PosixException where
    show = exception

instance Exception PosixException

access_mode :: ACCESS_MODE -> CInt
access_mode O_RDONLY = 0
access_mode O_WRONLY = 1
access_mode O_RDWR = 2

foreign import capi safe "fcntl.h open"
    raw_open :: Ptr CChar -> CInt -> IO CInt

open :: String -> ACCESS_MODE -> IO FD
open fp mode = do
    let imode = fromIntegral $ access_mode mode :: CInt
    fd <- withCString fp (\x -> raw_open x imode) :: IO CInt
    if (fromIntegral fd :: Int) < 0 then (throw $ PosixException (printf "Could not read file '%s'. Are you sure you have the permissions?" fp)) else return ()
    return (fromIntegral fd)

foreign import capi safe "unistd.h close"
    raw_close :: CInt -> IO CInt

close :: FD -> IO Int
close fd = do
    status <- raw_close (fromIntegral fd)
    if (fromIntegral status :: Int) < 0 then (throw $ PosixException (printf "Could not close file with pid %d." fd)) else return ()
    return (fromIntegral status)

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

foreign import capi safe "helper.h get_path_max"
    raw_maxpath :: CInt

maxPath :: Int
maxPath = fromIntegral raw_maxpath

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

-- raw_ptrace = undefined
