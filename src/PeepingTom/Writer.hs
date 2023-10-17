module PeepingTom.Writer (
    Writer,
    writeInt,
) where

import qualified Data.ByteString as BS
import qualified PeepingTom.Conversions as Conversions
import PeepingTom.Filters
import PeepingTom.Type

type Writer = Type -> BS.ByteString

writeInt :: Integer -> Writer
writeInt i t
    | isI8 t = Conversions.i8ToBS (fromIntegral i)
    | isI16 t = Conversions.i16ToBS (fromIntegral i)
    | isI32 t = Conversions.i32ToBS (fromIntegral i)
    | isI64 t = Conversions.i64ToBS (fromIntegral i)
    | isU8 t = Conversions.u8ToBS (fromIntegral i)
    | isU16 t = Conversions.i8ToBS (fromIntegral i)
    | isU32 t = Conversions.i8ToBS (fromIntegral i)
    | isU64 t = Conversions.i8ToBS (fromIntegral i)
    | otherwise = BS.empty
