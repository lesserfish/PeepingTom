module PeepingTom.Fast.Common (
    decodeTypes,
    getBSPtr,
) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.C
import Foreign.Ptr
import GHC.ForeignPtr
import PeepingTom.Type

decodeTypes :: CUInt -> Int -> [Type]
decodeTypes x s =
    i8
        ++ i16
        ++ i32
        ++ i64
        ++ u8
        ++ u16
        ++ u32
        ++ u64
        ++ f
        ++ d
        ++ b
  where
    i8 = if testBit x 0 then [Int8] else []
    i16 = if testBit x 1 then [Int16] else []
    i32 = if testBit x 2 then [Int32] else []
    i64 = if testBit x 3 then [Int64] else []
    u8 = if testBit x 4 then [UInt8] else []
    u16 = if testBit x 5 then [UInt16] else []
    u32 = if testBit x 6 then [UInt32] else []
    u64 = if testBit x 7 then [UInt64] else []
    f = if testBit x 8 then [Flt] else []
    d = if testBit x 9 then [Dbl] else []
    b = if testBit x 10 then [Bytes s] else []

getBSPtr :: BS.ByteString -> Ptr CChar
getBSPtr bs = castPtr ptr
  where
    (fptr, _) = BSI.toForeignPtr0 bs
    ptr = unsafeForeignPtrToPtr fptr
