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
writeInt i Int8 = Conversions.i8ToBS (fromIntegral i)
writeInt i Int16 = Conversions.i16ToBS (fromIntegral i)
writeInt i Int32 = Conversions.i32ToBS (fromIntegral i)
writeInt i Int64 = Conversions.i64ToBS (fromIntegral i)
writeInt i UInt8 = Conversions.u8ToBS (fromIntegral i)
writeInt i UInt16 = Conversions.u16ToBS (fromIntegral i)
writeInt i UInt32 = Conversions.u32ToBS (fromIntegral i)
writeInt i UInt64 = Conversions.u64ToBS (fromIntegral i)
writeInt _ _ = BS.empty
