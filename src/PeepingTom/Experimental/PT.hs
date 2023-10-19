{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PeepingTom.Raw.PT where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.List (intersperse, reverse)
import Data.Maybe
import Foreign.C
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified PeepingTom.Conversions as Conversions
import qualified PeepingTom.Filters as Filters
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.Posix as Posix
import qualified PeepingTom.Type as T
import Text.Printf

-- Helper Functions
openFile :: FilePath -> Posix.ACCESS_MODE -> IO FD
openFile fp mode = do
    fd <- Posix.open fp mode
    return fd

closeFile :: FD -> IO ()
closeFile fd = do
    _ <- Posix.close fd
    return ()

bsToString :: BS.ByteString -> String
bsToString bs = concat $ intersperse " " (fmap (printf "0x%02X") (BS.unpack bs))

maxSizeOf :: [T.Type] -> Size
maxSizeOf types = foldr max 0 (map T.sizeOf types)

-- pread
foreign import capi safe "unistd.h pread"
    raw_pread :: CInt -> Ptr a -> CSize -> CSize -> IO CSize

pread :: CStringLen -> FD -> Address -> IO ()
pread (cptr, clen) fd addr = do
    let cfd = fromIntegral fd :: CInt
    let caddr = fromIntegral addr :: CSize
    let ccount = fromIntegral clen :: CSize
    cbytes_read <- raw_pread cfd cptr ccount caddr
    let bytes_read = fromIntegral cbytes_read :: Int
    if bytes_read /= clen then error (printf "Could not read %d bytes of address %d. Read %d." clen addr bytes_read) else return ()

-- Chunk
data Chunk = Chunk {cAddr :: Address, cData :: CStringLen}
type CInterface a = Address -> Size -> (Chunk -> IO a) -> IO a

cInterface :: FD -> CInterface a
cInterface fd addr size action = do
    cdata <- newCStringLen (replicate size '0')
    pread cdata fd addr
    let chunk = Chunk addr cdata
    output <- action chunk
    let (cptr, clen) = cdata
    free cptr
    return $ output

withCInterface :: FD -> (CInterface a -> IO b) -> IO b
withCInterface filepath action = do
    let cinterface = cInterface filepath
    output <- action cinterface
    return output

withFileOpen :: FilePath -> Posix.ACCESS_MODE -> (FD -> IO a) -> IO a
withFileOpen filepath am action = do
    fd <- openFile filepath am
    output <- action fd
    closeFile fd
    return output

-- Comparison between Pointers
--
eqPtr :: (Storable a, Eq a) => Ptr a -> Ptr a -> IO Bool
eqPtr xptr yptr = do
    x <- peek xptr
    y <- peek yptr
    return $ x == y

shiftN :: Int -> Ptr a -> Ptr a
shiftN n ptr = plusPtr ptr n

eqPtrN :: (Storable a, Eq a) => Int -> Ptr a -> Ptr a -> IO Bool
eqPtrN n xptr yptr
    | n == 0 = return True
    | n < 0 = error "Error in eqPtrN. N needs to be positive"
    | otherwise = do
        byte_ok <- eqPtr xptr yptr
        if not byte_ok
            then return False
            else eqPtrN (n - 1) (shiftN 1 xptr) (shiftN 1 yptr)

shiftCStringLenN :: Int -> CStringLen -> CStringLen
shiftCStringLenN n (cptr, clen)
    | clen < n = error "Can't shift Cstring len that many bytes. It is not that large"
    | otherwise = (cptr', clen')
  where
    cptr' = shiftN n cptr
    clen' = clen - n

shiftChunkN :: Int -> Chunk -> Chunk
shiftChunkN n (Chunk caddr cdata) = Chunk (caddr + n) (shiftCStringLenN n cdata)

takeCSL :: Int -> CStringLen -> CStringLen
takeCSL n (cptr, clen) = (cptr, size)
  where
    size = min n clen

takeCSLMaybe :: Int -> CStringLen -> Maybe CStringLen
takeCSLMaybe n (cptr, clen)
    | n <= clen = Just (cptr, n)
    | otherwise = Nothing

eqCStringLenN :: Int -> CStringLen -> CStringLen -> IO Bool
eqCStringLenN n (xptr, xlen) (yptr, ylen)
    | xlen < n = return False
    | ylen < n = return False
    | otherwise = eqPtrN n xptr yptr

-- Filters

type Filter = T.Type -> CStringLen -> IO Bool

allFilter :: Filter
allFilter _ _ = return True

eqFilter :: CStringLen -> Filter
eqFilter (xptr, xlen) _ (yptr, ylen)
    | xlen /= ylen = return False
    | otherwise = eqPtrN xlen xptr yptr

eqInt32' :: Int -> (CStringLen -> IO a) -> IO a
eqInt32' value action = do
    let bs = Conversions.i32ToBS (fromIntegral value)
    BS.useAsCStringLen bs action

eqInt32 :: Int -> (Filter -> IO a) -> IO a
eqInt32 value action = do
    let bs = Conversions.i32ToBS (fromIntegral value)
    output <-
        BS.useAsCStringLen
            bs
            ( \intcstring -> do
                let fltr = eqFilter intcstring
                output <- action fltr
                return output
            )
    return output

eqInt' :: CStringLen -> Filter
eqInt' xstr t ystr
    | Filters.isI8 t = eqCStringLenN 1 xstr ystr
    | Filters.isI16 t = eqCStringLenN 2 xstr ystr
    | Filters.isI32 t = eqCStringLenN 4 xstr ystr
    | Filters.isI64 t = eqCStringLenN 8 xstr ystr
    | otherwise = return False

eqInt :: Int -> (Filter -> IO a) -> IO a
eqInt value action = do
    let bs = Conversions.i64ToBS (fromIntegral value)
    output <-
        BS.useAsCStringLen
            bs
            ( \intcstring -> do
                let fltr = eqInt' intcstring
                output <- action fltr
                return output
            )
    return output

-- Let's do some tests

type Candidate = (Address, BS.ByteString)

acceptType :: Filter -> Chunk -> T.Type -> IO (Maybe T.Type)
acceptType fltr chunk t = do
    let size = T.sizeOf t
    let cslm = takeCSLMaybe size (cData chunk)
    case cslm of
        Nothing -> return Nothing
        Just csl -> do
            ok <- fltr t csl
            if ok then return $ Just t else return Nothing

extractAddr :: [T.Type] -> Filter -> Chunk -> IO (Maybe Candidate)
extractAddr types fltr chunk = do
    let cdata = cData chunk :: CStringLen
    type_test <- mapM (acceptType fltr chunk) types :: IO [Maybe T.Type]
    let accepted_types = catMaybes type_test
    if (length accepted_types) > 0
        then do
            let addr = cAddr chunk
            let max_size = maxSizeOf accepted_types
            let short_cl = takeCSL max_size cdata
            bs <- BS.packCStringLen short_cl
            -- putStrLn $ printf "Accepted address %8x with Data equal to %s [%s]" (addr) (bsToString bs) (show accepted_types)
            return $ Just (addr, bs)
        else do
            return $ Nothing

extractAddresses :: Address -> [T.Type] -> Filter -> Chunk -> IO [Candidate]
extractAddresses maxoffset types fltr chunk = do
    let offsets = [0 .. maxoffset] :: [Int]
    let chunks = map (\n -> shiftChunkN n chunk) offsets :: [Chunk]
    addr_test <- mapM (extractAddr types fltr) chunks :: IO [Maybe Candidate]
    let candidates = catMaybes addr_test
    return candidates

extractRegionHelper :: CInterface [Candidate] -> [T.Type] -> Filter -> (Address, Address) -> Size -> IO [Candidate]
extractRegionHelper cif types fltr (start_address, end_address) chunk_size = do
    if start_address >= end_address
        then return []
        else do
            let max_size = maxSizeOf types
            let offset_size = min (end_address - start_address) chunk_size
            let read_size = min (end_address - start_address) (chunk_size + max_size)
            tail <- extractRegionHelper cif types fltr (start_address + offset_size, end_address) chunk_size
            candidates <-
                cif
                    start_address
                    read_size
                    ( \chunk -> do
                        candidates <- extractAddresses offset_size types fltr chunk
                        return candidates
                    )
            evaluate candidates
            return $ candidates ++ tail

extractRegion :: CInterface [Candidate] -> [T.Type] -> Filter -> Maps.Region -> Size -> IO [Candidate]
extractRegion rinterface types fltr region chunk_size = do
    let sa = Maps.rStartAddr region
    let ea = Maps.rEndAddr region
    candidates <- extractRegionHelper rinterface types fltr (sa, ea) chunk_size
    putStrLn $ printf "Extracted %4d candidates from Region. (size: %8x" (length candidates) (Maps.rEndAddr region - Maps.rStartAddr region)
    return candidates

scanMapHelper :: Size -> [T.Type] -> Filter -> Maps.MapInfo -> IO [Candidate]
scanMapHelper chunk_size types fltr map = do
    let pid = Maps.miPID map
    let regions = Maps.miRegions map :: [Maps.Region]
    let filepath = printf "/proc/%d/mem" pid
    candidates <-
        withFileOpen
            filepath
            Posix.O_RDONLY
            ( \fd -> do
                withCInterface
                    fd
                    ( \cinterface -> do
                        cc <- forM regions (\reg -> extractRegion cinterface types fltr reg chunk_size) :: IO [[Candidate]]
                        let candidates = concat cc
                        return candidates
                    )
            )
    return candidates

intTypes :: [T.Type]
intTypes = [(T.Type T.Int8), (T.Type T.Int16), (T.Type T.Int32), (T.Type T.Int64)]

debug2 :: IO ()
debug2 = do
    let pid = 309437 :: PID
    putStrLn $ printf "PID: %d" pid
    all_maps <- Maps.getMapInfo pid
    let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
    candidates <-
        eqInt
            41
            ( \fltr -> do
                candidates <- scanMapHelper 10000 intTypes fltr maps
                return candidates
            )
    putStrLn $ printf "\n\n\nExtracted a total of %d candidates!" (length candidates)
    return ()

debug1 :: IO ()
debug1 = do
    eqInt
        0x73
        ( \fltr -> do
            withFileOpen
                "/home/lesserfish/Documents/tmp/magic.txt"
                Posix.O_RDONLY
                ( \fd -> do
                    withCInterface
                        fd
                        ( \cinterface -> do
                            cinterface
                                0
                                36
                                ( \chunk -> do
                                    extractAddresses 30 intTypes fltr chunk
                                    return ()
                                )
                        )
                )
        )

debug0 :: IO ()
debug0 = do
    withFileOpen
        "/home/lesserfish/Documents/tmp/magic.txt"
        Posix.O_RDONLY
        ( \fd -> do
            withCInterface
                fd
                ( \cinterface -> do
                    cinterface
                        0
                        60
                        ( \chunk -> do
                            let cstringlen = cData chunk
                            bs <- BS.packCStringLen cstringlen
                            putStrLn $ bsToString bs
                            let cstringlen2 = shiftCStringLenN 2 cstringlen
                            bs2 <- BS.packCStringLen cstringlen2
                            putStrLn $ bsToString bs2
                            return ()
                        )
                    return ()
                )
        )
