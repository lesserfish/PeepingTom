{-# LANGUAGE BangPatterns #-}

module PeepingTom.Filters (
    Filter,
    FilterInfo,
    compareBS,
    eqInt,
    eqInt',
    eqIntX,
    compareInt,
) where

import qualified Data.ByteString as BS
import Data.Functor ((<$>))
import qualified Data.Int as I
import Data.List (any, intersperse)
import Data.Maybe (Maybe, fromMaybe)
import Data.Typeable (Typeable, typeOf)
import Debug.Trace
import PeepingTom.Conversions
import PeepingTom.Type
import Text.Printf

type FilterInfo = (Filter, Int)
type Filter = BS.ByteString -> [Type]

castInteger :: Type -> BS.ByteString -> Maybe Integer
castInteger Int8 b = Just . toInteger $ i8FromBS b
castInteger Int16 b = Just . toInteger $ i16FromBS b
castInteger Int32 b = Just . toInteger $ i32FromBS b
castInteger Int64 b = Just . toInteger $ i64FromBS b
castInteger UInt8 b = Just . toInteger $ u8FromBS b
castInteger UInt16 b = Just . toInteger $ u16FromBS b
castInteger UInt32 b = Just . toInteger $ u32FromBS b
castInteger UInt64 b = Just . toInteger $ u64FromBS b
castInteger _ _ = Nothing

ifLargeEnough :: Type -> BS.ByteString -> Maybe a -> Maybe a
ifLargeEnough t bs a = if (BS.length bs) < (sizeOf t) then Nothing else a

compareInt' :: (Integer -> Bool) -> BS.ByteString -> Type -> Maybe Bool
compareInt' fltr bs t = fltr <$> (ifLargeEnough t bs (castInteger t bs))

bsToString :: BS.ByteString -> String
bsToString bs = concat $ intersperse " " (fmap (printf "0x%02X") (BS.unpack bs))

-- compareInt :: (Integer -> Bool) -> Filter
-- compareInt fltr bs = output
--  where
--    output = fromMaybe False (compareInt' fltr bs t)

-- ByteString
eqBS :: BS.ByteString -> Filter
eqBS bs1 bs2 = if bs1 == bs2 then [byteType] else []
  where
    byteType = Bytes (BS.length bs1)

compareBS :: (BS.ByteString -> Bool) -> Filter
compareBS fltr bs = if fltr bs then [byteType] else []
  where
    byteType = Bytes (BS.length bs)

-- Integer Filters
--
eqBSN' :: Int -> BS.ByteString -> BS.ByteString -> Bool
eqBSN' 0 _ _ = True
eqBSN' n !bsx bsy
    | bx == by = eqBSN (n - 1) (BS.tail bsx) (BS.tail bsy)
    | otherwise = False
  where
    !bx = BS.head bsx
    by = BS.head bsy

eqBSN :: Int -> BS.ByteString -> BS.ByteString -> Bool
eqBSN n bsx bsy
    | (BS.length bsy) < n = False
    | otherwise = eqBSN' n bsx bsy

sequenceF :: [Filter] -> Filter
sequenceF [] _ = []
sequenceF (f : rest) bs = if flen == 0 then [] else (f bs) ++ sequenceF rest bs
  where
    flen = length $ f bs

i8Eq :: BS.ByteString -> Filter
i8Eq !ibs bs
    | BS.length bs < 1 = []
    | otherwise = if (eqBSN 1 ibs bs) then [Int8] else []

i16Eq :: BS.ByteString -> Filter
i16Eq !ibs bs
    | BS.length bs < 2 = []
    | otherwise = if (eqBSN 2 ibs bs) then [Int16] else []

i32Eq :: BS.ByteString -> Filter
i32Eq !ibs bs
    | BS.length bs < 4 = []
    | otherwise = if (eqBSN 4 ibs bs) then [Int32] else []

i64Eq :: BS.ByteString -> Filter
i64Eq !ibs bs
    | BS.length bs < 8 = []
    | otherwise = if (eqBSN 8 ibs bs) then [Int64] else []

feqInt :: Integer -> Filter
feqInt value = sequenceF [i8Eq i8, i16Eq i16, i32Eq i32, i64Eq i64]
  where
    !i8 = i8ToBS (fromInteger value)
    !i16 = i16ToBS (fromInteger value)
    !i32 = i32ToBS (fromInteger value)
    !i64 = i64ToBS (fromInteger value)

eqInt :: Integer -> FilterInfo
eqInt x = (feqInt x, 8)

feqIntX :: (Bool, Bool, Bool, Bool) -> Integer -> Filter
feqIntX (i8check, i16check, i32check, i64check) value = sequenceF (i8f ++ i16f ++ i32f ++ i64f)
  where
    !i8 = i8ToBS (fromInteger value)
    !i16 = i16ToBS (fromInteger value)
    !i32 = i32ToBS (fromInteger value)
    !i64 = i64ToBS (fromInteger value)
    !i8f = if i8check then [i8Eq i8] else []
    !i16f = if i16check then [i16Eq i16] else []
    !i32f = if i32check then [i32Eq i32] else []
    !i64f = if i64check then [i64Eq i64] else []

eqIntX :: (Bool, Bool, Bool, Bool) -> Integer -> FilterInfo
eqIntX b v = (feqIntX b v, s)
  where
    (b1, b2, b3, b4) = b
    s = if b4 then 8 else if b3 then 4 else if b2 then 2 else if b1 then 1 else 0

feqInt' :: Integer -> Filter
feqInt' x
    | x >= fromIntegral (minBound :: I.Int8) && x <= fromIntegral (maxBound :: I.Int8) = feqIntX (True, True, True, True) x
    | x >= fromIntegral (minBound :: I.Int16) && x <= fromIntegral (maxBound :: I.Int16) = feqIntX (False, True, True, True) x
    | x >= fromIntegral (minBound :: I.Int32) && x <= fromIntegral (maxBound :: I.Int32) = feqIntX (False, False, True, True) x
    | x >= fromIntegral (minBound :: I.Int64) && x <= fromIntegral (maxBound :: I.Int64) = feqIntX (False, False, False, True) x
    | otherwise = \_ -> []

eqInt' :: Integer -> FilterInfo
eqInt' x = (feqInt' x, 8)

fcompareInt :: (Integer -> Bool) -> Filter
fcompareInt cmp bs = i8 ++ i16 ++ i32 ++ i64
  where
    i8 = if (BS.length bs) < 1 then [] else c8
    c8 = if cmp . fromIntegral $ i8FromBS bs then [Int8] else []
    i16 = if (BS.length bs) < 2 then [] else c16
    c16 = if cmp . fromIntegral $ i16FromBS bs then [Int16] else []
    i32 = if (BS.length bs) < 4 then [] else c32
    c32 = if cmp . fromIntegral $ i32FromBS bs then [Int32] else []
    i64 = if (BS.length bs) < 8 then [] else c64
    c64 = if cmp . fromIntegral $ i64FromBS bs then [Int64] else []

compareInt :: (Integer -> Bool) -> FilterInfo
compareInt c = (fcompareInt c, 8)
