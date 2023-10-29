module PeepingTom.Experimental.Fast.Common where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.C
import Foreign.Ptr
import GHC.ForeignPtr
import qualified PeepingTom.Type as T

decodeTypes :: CUInt -> Int -> [T.Type]
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
    i8 = if testBit x 0 then [T.Int8] else []
    i16 = if testBit x 1 then [T.Int16] else []
    i32 = if testBit x 2 then [T.Int32] else []
    i64 = if testBit x 3 then [T.Int64] else []
    u8 = if testBit x 4 then [T.UInt8] else []
    u16 = if testBit x 5 then [T.UInt16] else []
    u32 = if testBit x 6 then [T.UInt32] else []
    u64 = if testBit x 7 then [T.UInt64] else []
    f = if testBit x 8 then [T.Flt] else []
    d = if testBit x 9 then [T.Dbl] else []
    b = if testBit x 10 then [T.Bytes s] else []

getBSPtr :: BS.ByteString -> Ptr CChar
getBSPtr bs = castPtr ptr
  where
    (fptr, _) = BSI.toForeignPtr0 bs
    ptr = unsafeForeignPtrToPtr fptr
