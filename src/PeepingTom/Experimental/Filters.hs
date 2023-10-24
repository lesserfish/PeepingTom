{-# LANGUAGE BangPatterns #-}

module PeepingTom.Experimental.Filters (
    eqInt,
    Filter (..),
    scanMapS,
    scanMap,
) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Int as I
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Word as W
import Foreign.Ptr
import Foreign.Storable (peek)
import qualified GHC.ForeignPtr as P
import qualified PeepingTom.IO as IO
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.State as ST
import qualified PeepingTom.Type as T
import Text.Printf (printf)

toI8 :: BS.ByteString -> IO (Maybe I.Int8)
toI8 bs = do
    let (fptr, size) = BSI.toForeignPtr0 bs
    if size < 1
        then do
            return Nothing
        else do
            let ifptr = P.castForeignPtr fptr :: P.ForeignPtr I.Int8
            let iptr = P.unsafeForeignPtrToPtr ifptr :: Ptr I.Int8
            ival <- peek iptr :: IO I.Int8
            return $ Just ival

toI16 :: BS.ByteString -> IO (Maybe I.Int16)
toI16 bs = do
    let (fptr, size) = BSI.toForeignPtr0 bs
    if size < 2
        then do
            return Nothing
        else do
            let ifptr = P.castForeignPtr fptr :: P.ForeignPtr I.Int16
            let iptr = P.unsafeForeignPtrToPtr ifptr :: Ptr I.Int16
            ival <- peek iptr :: IO I.Int16
            return $ Just ival

toI32 :: BS.ByteString -> IO (Maybe I.Int32)
toI32 bs = do
    let (fptr, size) = BSI.toForeignPtr0 bs
    if size < 4
        then do
            return Nothing
        else do
            let ifptr = P.castForeignPtr fptr :: P.ForeignPtr I.Int32
            let iptr = P.unsafeForeignPtrToPtr ifptr :: Ptr I.Int32
            ival <- peek iptr :: IO I.Int32
            return $ Just ival

toI64 :: BS.ByteString -> IO (Maybe I.Int64)
toI64 bs = do
    let (fptr, size) = BSI.toForeignPtr0 bs
    if size < 8
        then do
            return Nothing
        else do
            let ifptr = P.castForeignPtr fptr :: P.ForeignPtr I.Int64
            let iptr = P.unsafeForeignPtrToPtr ifptr :: Ptr I.Int64
            ival <- peek iptr :: IO I.Int64
            return $ Just ival

type Filter = BS.ByteString -> T.Type -> IO Bool

testIBS' :: (I.Int8, I.Int16, I.Int32, I.Int64) -> Filter
testIBS' (i8, _, _, _) bs T.Int8 = do
    mi8' <- (toI8 bs)
    case mi8' of
        Nothing -> return False
        Just i8' -> return (i8 == i8')
testIBS' (_, i16, _, _) bs T.Int16 = do
    mi16' <- (toI16 bs)
    case mi16' of
        Nothing -> return False
        Just i16' -> return (i16 == i16')
testIBS' (_, _, i32, _) bs T.Int32 = do
    mi32' <- (toI32 bs)
    case mi32' of
        Nothing -> return False
        Just i32' -> return (i32 == i32')
testIBS' (_, _, _, i64) bs T.Int64 = do
    mi64' <- (toI64 bs)
    case mi64' of
        Nothing -> return False
        Just i64' -> return (i64 == i64')

eqInt :: Integer -> Filter
eqInt value = testIBS' (i8, i16, i32, i64)
  where
    !i8 = fromIntegral value
    !i16 = fromIntegral value
    !i32 = fromIntegral value
    !i64 = fromIntegral value

-- IO Scan methods

maxSizeOf :: [T.Type] -> Size
maxSizeOf types = output
  where
    output = foldr max 0 (map T.sizeOf types)

mFilter :: Filter -> BS.ByteString -> T.Type -> IO (Maybe T.Type)
mFilter fltr bs t = do
    ok <- fltr bs t :: IO Bool
    if ok then return $ Just t else return $ Nothing

filterAddress :: [T.Type] -> Filter -> Int -> IO.MemoryChunk -> Address -> IO (Maybe ST.Candidate)
filterAddress types fltr regid chunk offset = do
    let address = (IO.mcStartAddr chunk) + offset
    let bytes = BS.drop offset (IO.mcData chunk) -- Get all the bytes starting from offset
    mcandidate_types <- mapM (mFilter fltr bytes) types :: IO [Maybe T.Type]
    let candidate_types = catMaybes mcandidate_types
    if length candidate_types == 0
        then return Nothing
        else do
            let max_size = maxSizeOf candidate_types
            let byte_data = BS.take max_size bytes -- Store this amount of bytes
            let new_candidate =
                    ST.Candidate
                        { ST.cAddress = address
                        , ST.cData = byte_data
                        , ST.cTypes = candidate_types
                        , ST.cRegionID = regid
                        }
            return $ Just new_candidate

regionScanHelper :: IO.RInterface -> [T.Type] -> Filter -> Size -> (Address, Address) -> ST.ScanOptions -> IO [ST.Candidate]
regionScanHelper rinterface types fltr regid (start_address, end_address) scopt = do
    if start_address >= end_address
        then return []
        else do
            let chunk_size = ST.soChunkSize scopt
            let max_size = maxSizeOf types
            let offset_size = min (end_address - start_address) chunk_size
            let read_size = min (end_address - start_address) (chunk_size + max_size)
            let offset = [0 .. offset_size]
            tail <- regionScanHelper rinterface types fltr regid (start_address + offset_size + 1, end_address) scopt
            chunk <- rinterface start_address read_size
            mcandidates <- mapM (filterAddress types fltr regid chunk) offset :: IO [Maybe ST.Candidate]
            let candidates = catMaybes mcandidates :: [ST.Candidate]
            evaluate candidates
            return $ candidates ++ tail

regionScan :: IO.RInterface -> [T.Type] -> Filter -> Maps.Region -> ST.ScanOptions -> IO [ST.Candidate]
regionScan rinterface types fltr region scopt = do
    let rid = Maps.rID region
    let sa = Maps.rStartAddr region
    let ea = Maps.rEndAddr region
    candidates <- regionScanHelper rinterface types fltr rid (sa, ea) scopt
    return candidates

regionScanLog :: Maps.Region -> [ST.Candidate] -> IO ()
regionScanLog reg result = do
    if length result == 0 then return () else putStrLn $ printf "Extracted %4d candidates from Region %4d (size = %8x)" (length result) (Maps.rID reg) ((Maps.rEndAddr reg) - (Maps.rStartAddr reg))

vocal :: (a -> b -> IO ()) -> (a -> IO b) -> a -> (IO b)
vocal log action input = do
    output <- action input
    log input output
    return output

scanMapHelper :: ST.ScanOptions -> [T.Type] -> Filter -> Maps.MapInfo -> IO [ST.Candidate]
scanMapHelper scopt types fltr map = do
    let stopsig = ST.soSIGSTOP scopt
    let pid = Maps.miPID map
    let regions = Maps.miRegions map
    candidates <-
        IO.withRInterface
            pid
            stopsig
            ( \rinterface -> do
                let action = vocal regionScanLog (\x -> regionScan rinterface types fltr x scopt) :: Maps.Region -> IO [ST.Candidate]
                fc <- forM regions action :: IO [[ST.Candidate]]
                return $ concat fc
            )
    return $ candidates

scanMapS :: ST.ScanOptions -> [T.Type] -> Filter -> Maps.MapInfo -> IO ST.PeepState
scanMapS scopt types fltr map = do
    let pid = (Maps.miPID map)
    candidates <- scanMapHelper scopt types fltr map
    return ST.PeepState{ST.psPID = pid, ST.psCandidates = candidates, ST.psRegions = map}

scanMap :: [T.Type] -> Filter -> Maps.MapInfo -> IO ST.PeepState
scanMap = scanMapS ST.defaultScanOptions
