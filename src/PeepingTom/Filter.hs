{-# LANGUAGE BangPatterns #-}

module PeepingTom.Filter (
    Filter,
    FilterInfo,
    compareBS,
    eqInt,
    eqInt',
    eqIntX,
    compareInt,
    compareIntX,
    i8Eq,
    i16Eq,
    i32Eq,
    i64Eq,
    eqStr,
    eqBS,
    applyFilter,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Functor ((<$>))
import qualified Data.Int as I
import Data.List (any, intersperse)
import Data.Maybe (Maybe, fromMaybe, mapMaybe)
import Data.Typeable (Typeable, typeOf)
import Debug.Trace
import PeepingTom.Common
import PeepingTom.Conversions
import PeepingTom.State
import PeepingTom.Type
import Text.Printf

type FilterInfo = (Filter, Int)
type Filter = BS.ByteString -> [Type]

applyFilter :: FilterInfo -> PeepState -> PeepState
applyFilter fltr state = output
  where
    matchs' = mapMaybe (matchFilter fltr) (psMatchs state)
    output = state{psMatchs = matchs'}

matchFilter :: FilterInfo -> Match -> Maybe Match
matchFilter (fltr, _) match = output
  where
    bsData = mData $ match :: BS.ByteString
    valid_types = fltr bsData :: [Type]
    output = if length valid_types == 0 then Nothing else match'
    max_size = maxSizeOf valid_types
    bsData' = BS.take max_size bsData -- Only store that amount of bytes
    match' = Just match{mData = bsData', mTypes = valid_types}

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

bsToString :: BS.ByteString -> String
bsToString bs = concat $ intersperse " " (fmap (printf "0x%02X") (BS.unpack bs))

-- ByteString
eqBS' :: BS.ByteString -> Filter
eqBS' !bs1 bs2 = if (eqBSN (BS.length bs1) bs1 bs2) then [byteType] else []
  where
    byteType = Bytes (BS.length bs1)

eqBS :: BS.ByteString -> FilterInfo
eqBS bs = (eqBS' bs, s)
  where
    s = BS.length bs

compareBS :: (BS.ByteString -> Bool) -> Filter
compareBS fltr bs = if fltr bs then [byteType] else []
  where
    byteType = Bytes (BS.length bs)

eqStr' :: String -> Filter
eqStr' str bs2
    | eqBSN l bs1 bs2 = [byteType]
    | otherwise = []
  where
    !bs1 = BSC.pack str
    !l = length str
    !byteType = Bytes (length str)

eqStr :: String -> FilterInfo
eqStr bs = (eqStr' bs, length bs)

-- Integer Filters
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

i8Eq' :: BS.ByteString -> Filter
i8Eq' !ibs bs
    | BS.length bs < 1 = []
    | otherwise = if (eqBSN 1 ibs bs) then [Int8] else []

fi8Eq :: I.Int8 -> Filter
fi8Eq value = i8Eq' bs
  where
    !bs = i8ToBS value

i8Eq :: I.Int8 -> FilterInfo
i8Eq x = (fi8Eq x, 1)

i16Eq' :: BS.ByteString -> Filter
i16Eq' !ibs bs
    | BS.length bs < 2 = []
    | otherwise = if (eqBSN 2 ibs bs) then [Int16] else []

fi16Eq :: I.Int16 -> Filter
fi16Eq value = i16Eq' bs
  where
    !bs = i16ToBS value

i16Eq :: I.Int16 -> FilterInfo
i16Eq x = (fi16Eq x, 2)

i32Eq' :: BS.ByteString -> Filter
i32Eq' !ibs bs
    | BS.length bs < 4 = []
    | otherwise = if (eqBSN 4 ibs bs) then [Int32] else []

fi32Eq :: I.Int32 -> Filter
fi32Eq value = i32Eq' bs
  where
    !bs = i32ToBS value

i32Eq :: I.Int32 -> FilterInfo
i32Eq x = (fi32Eq x, 4)

i64Eq' :: BS.ByteString -> Filter
i64Eq' !ibs bs
    | BS.length bs < 8 = []
    | otherwise = if (eqBSN 8 ibs bs) then [Int64] else []

fi64Eq :: I.Int64 -> Filter
fi64Eq value = i64Eq' bs
  where
    !bs = i64ToBS value

i64Eq :: I.Int64 -> FilterInfo
i64Eq x = (fi64Eq x, 8)

feqInt' :: Integer -> Filter
feqInt' value = sequenceF [i8Eq' i8, i16Eq' i16, i32Eq' i32, i64Eq' i64]
  where
    !i8 = i8ToBS (fromInteger value)
    !i16 = i16ToBS (fromInteger value)
    !i32 = i32ToBS (fromInteger value)
    !i64 = i64ToBS (fromInteger value)

eqInt' :: Integer -> FilterInfo
eqInt' x = (feqInt x, 8)

feqIntX :: (Bool, Bool, Bool, Bool) -> Integer -> Filter
feqIntX (i8check, i16check, i32check, i64check) value = sequenceF (i8f ++ i16f ++ i32f ++ i64f)
  where
    !i8 = i8ToBS (fromInteger value)
    !i16 = i16ToBS (fromInteger value)
    !i32 = i32ToBS (fromInteger value)
    !i64 = i64ToBS (fromInteger value)
    !i8f = if i8check && i8Range value then [i8Eq' i8] else []
    !i16f = if i16check && i16Range value then [i16Eq' i16] else []
    !i32f = if i32check && i32Range value then [i32Eq' i32] else []
    !i64f = if i64check && i64Range value then [i64Eq' i64] else []

eqIntX :: (Bool, Bool, Bool, Bool) -> Integer -> FilterInfo
eqIntX b v = (feqIntX b v, s)
  where
    (b1, b2, b3, b4) = b
    s = if b4 then 8 else if b3 then 4 else if b2 then 2 else if b1 then 1 else 0

i8Range :: Integer -> Bool
i8Range x = x >= fromIntegral (minBound :: I.Int8) && x <= fromIntegral (maxBound :: I.Int8)

i16Range :: Integer -> Bool
i16Range x = x >= fromIntegral (minBound :: I.Int16) && x <= fromIntegral (maxBound :: I.Int16)

i32Range :: Integer -> Bool
i32Range x = x >= fromIntegral (minBound :: I.Int32) && x <= fromIntegral (maxBound :: I.Int32)

i64Range :: Integer -> Bool
i64Range x = x >= fromIntegral (minBound :: I.Int64) && x <= fromIntegral (maxBound :: I.Int64)

feqInt :: Integer -> Filter
feqInt x
    | i8Range x = feqIntX (True, True, True, True) x
    | i16Range x = feqIntX (False, True, True, True) x
    | i32Range x = feqIntX (False, False, True, True) x
    | i64Range x = feqIntX (False, False, False, True) x
    | otherwise = \_ -> []

eqInt :: Integer -> FilterInfo
eqInt x = (feqInt' x, 8)

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

fcompareIntX :: (Bool, Bool, Bool, Bool) -> (Integer -> Bool) -> Filter
fcompareIntX !(i8check, i16check, i32check, i64check) cmp bs = i8 ++ i16 ++ i32 ++ i64
  where
    i8 = if i8check && (BS.length bs) < 1 then [] else c8
    c8 = if cmp . fromIntegral $ i8FromBS bs then [Int8] else []
    i16 = if i16check && (BS.length bs) < 2 then [] else c16
    c16 = if cmp . fromIntegral $ i16FromBS bs then [Int16] else []
    i32 = if i32check && (BS.length bs) < 4 then [] else c32
    c32 = if cmp . fromIntegral $ i32FromBS bs then [Int32] else []
    i64 = if i64check && (BS.length bs) < 8 then [] else c64
    c64 = if cmp . fromIntegral $ i64FromBS bs then [Int64] else []

compareIntX :: (Bool, Bool, Bool, Bool) -> (Integer -> Bool) -> FilterInfo
compareIntX !(i8check, i16check, i32check, i64check) cmp = (fcompareIntX (i8check, i16check, i32check, i64check) cmp, s)
  where
    s = if i64check then 8 else if i32check then 4 else if i16check then 2 else if i8check then 1 else 0
