{-# LANGUAGE BangPatterns #-}

module PeepingTom.Filters (
    Filter,
    isI8,
    isI16,
    isI32,
    isI64,
    isU8,
    isU16,
    isU32,
    isU64,
    isFlt,
    isDbl,
    eqInt,
    compareBS,
    compareInt,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSUnsafe
import Data.Functor ((<$>))
import Data.List (any, intersperse)
import Data.Maybe (Maybe, fromMaybe)
import Data.Typeable (Typeable, typeOf)
import Debug.Trace
import PeepingTom.Conversions
import PeepingTom.Type
import Text.Printf

type Filter = BS.ByteString -> Type -> Bool

isI8 :: Filter
isI8 _ Int8 = True
isI8 _ _ = False

isI16 :: Filter
isI16 _ Int16 = True
isI16 _ _ = False

isI32 :: Filter
isI32 _ Int32 = True
isI32 _ _ = False

isI64 :: Filter
isI64 _ Int64 = True
isI64 _ _ = False

isU8 :: Filter
isU8 _ UInt8 = True
isU8 _ _ = False

isU16 :: Filter
isU16 _ UInt16 = True
isU16 _ _ = False

isU32 :: Filter
isU32 _ UInt32 = True
isU32 _ _ = False

isU64 :: Filter
isU64 _ UInt64 = True
isU64 _ _ = False

isFlt :: Filter
isFlt _ Flt = True
isFlt _ _ = False

isDbl :: Filter
isDbl _ Dbl = True
isDbl _ _ = False

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

compareInt :: (Integer -> Bool) -> Filter
compareInt fltr bs t = output
  where
    output = fromMaybe False (compareInt' fltr bs t)

-- ByteString
eqBS :: BS.ByteString -> Filter
eqBS bs1 bs2 _ = bs1 == bs2

compareBS :: (BS.ByteString -> Bool) -> Filter
compareBS fltr bs2 _ = fltr bs2

eqBSN :: Int -> BS.ByteString -> BS.ByteString -> Bool
eqBSN 0 _ _ = True
eqBSN n !bsx bsy
    | (BS.length bsy) < n = False
    | bx == by = eqBSN (n - 1) (BS.tail bsx) (BS.tail bsy)
    | otherwise = False
  where
    !bx = BS.head bsx
    by = BS.head bsy

testIBS :: (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString) -> Filter
testIBS (i8, i16, i32, i64) bs Int8 = i8 == (BS.take 1 bs)
testIBS (i8, i16, i32, i64) bs Int16 = i16 == (BS.take 2 bs)
testIBS (i8, i16, i32, i64) bs Int32 = i32 == (BS.take 4 bs)
testIBS (i8, i16, i32, i64) bs Int64 = i64 == (BS.take 8 bs)

testIBS' :: (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString) -> Filter
testIBS' (i8, i16, i32, i64) bs Int8 = (eqBSN 1 i8 bs)
testIBS' (i8, i16, i32, i64) bs Int16 = (eqBSN 2 i16 bs)
testIBS' (i8, i16, i32, i64) bs Int32 = (eqBSN 4 i32 bs)
testIBS' (i8, i16, i32, i64) bs Int64 = (eqBSN 8 i64 bs)

eqInt :: Integer -> Filter
eqInt value = testIBS' (i8, i16, i32, i64)
  where
    !i8 = i8ToBS (fromInteger value)
    !i16 = i16ToBS (fromInteger value)
    !i32 = i32ToBS (fromInteger value)
    !i64 = i64ToBS (fromInteger value)
