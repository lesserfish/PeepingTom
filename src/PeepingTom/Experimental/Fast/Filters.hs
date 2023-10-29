{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PeepingTom.Experimental.Fast.Filters where

import qualified Data.ByteString as BS
import qualified Data.Int as I
import Foreign.C
import Foreign.Ptr
import qualified PeepingTom.Conversions as Conv
import qualified PeepingTom.Type as T

data CFilter = CFilter
    { cfFPtr :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)
    , cfMaxSize :: Int
    , cfReference :: BS.ByteString
    }

foreign import capi safe "C/ChunkReader.c &i8_eq"
    c_i8eq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)

foreign import capi safe "C/ChunkReader.c &i16_eq"
    c_i16eq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)

foreign import capi safe "C/ChunkReader.c &i32_eq"
    c_i32eq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)

foreign import capi safe "C/ChunkReader.c &i64_eq"
    c_i64eq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)

foreign import capi safe "C/ChunkReader.c &int_eq"
    c_inteq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)

foreign import capi safe "C/ChunkReader.c &int16p_eq"
    c_int16peq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)

foreign import capi safe "C/ChunkReader.c &int32p_eq"
    c_int32peq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)

foreign import capi safe "C/ChunkReader.c &voidf"
    c_void :: FunPtr (Ptr CChar -> Ptr CChar -> CSize)

i8Eq :: Integer -> CFilter
i8Eq value = CFilter c_i8eq 1 bsData
  where
    !bsData = Conv.i8ToBS (fromInteger value)

i16Eq :: Integer -> CFilter
i16Eq value = CFilter c_i16eq 2 bsData
  where
    !bsData = Conv.i16ToBS (fromInteger value)

i32Eq :: Integer -> CFilter
i32Eq value = CFilter c_i32eq 4 bsData
  where
    !bsData = Conv.i32ToBS (fromInteger value)

i64Eq :: Integer -> CFilter
i64Eq value = CFilter c_i64eq 8 bsData
  where
    !bsData = Conv.i64ToBS (fromInteger value)

i8Range :: Integer -> Bool
i8Range x = x >= fromIntegral (minBound :: I.Int8) && x <= fromIntegral (maxBound :: I.Int8)

i16Range :: Integer -> Bool
i16Range x = x >= fromIntegral (minBound :: I.Int16) && x <= fromIntegral (maxBound :: I.Int16)

i32Range :: Integer -> Bool
i32Range x = x >= fromIntegral (minBound :: I.Int32) && x <= fromIntegral (maxBound :: I.Int32)

i64Range :: Integer -> Bool
i64Range x = x >= fromIntegral (minBound :: I.Int64) && x <= fromIntegral (maxBound :: I.Int64)

pickIntFunPtr :: Integer -> FunPtr (Ptr CChar -> Ptr CChar -> CSize)
pickIntFunPtr value
    | i8Range value = c_inteq
    | i16Range value = c_int16peq
    | i32Range value = c_int32peq
    | i64Range value = c_i64eq
    | otherwise = c_void

eqInt :: Integer -> CFilter
eqInt value = CFilter func 8 bsData
  where
    !bsData = Conv.i64ToBS (fromInteger value)
    !func = pickIntFunPtr value