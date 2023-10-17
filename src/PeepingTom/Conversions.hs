module PeepingTom.Conversions (
    i8ToBS,
    i8FromBS,
    i16ToBS,
    i16FromBS,
    i32ToBS,
    i32FromBS,
    i64ToBS,
    i64FromBS,
    u8ToBS,
    u8FromBS,
    u16ToBS,
    u16FromBS,
    u32ToBS,
    u32FromBS,
    u64ToBS,
    u64FromBS,
) where

import qualified Data.Binary.Get as BG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Int as Int
import qualified Data.Word as UInt
import qualified GHC.ByteOrder as Endian

-- Conversions to/from ByteString

-- Int.Int8
i8FromBS :: BS.ByteString -> Int.Int8
i8FromBS x = BG.runGet BG.getInt8 (BL.fromStrict x)

i8ToBS :: Int.Int8 -> BS.ByteString
i8ToBS x = BL.toStrict (BB.toLazyByteString . BB.int8 $ x)

-- Int.Int16
i16FromBSle :: BS.ByteString -> Int.Int16
i16FromBSle x = BG.runGet BG.getInt16le (BL.fromStrict x)

i16ToBSle :: Int.Int16 -> BS.ByteString
i16ToBSle x = BL.toStrict (BB.toLazyByteString . BB.int16LE $ x)

i16FromBSbe :: BS.ByteString -> Int.Int16
i16FromBSbe x = BG.runGet BG.getInt16be (BL.fromStrict x)

i16ToBSbe :: Int.Int16 -> BS.ByteString
i16ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.int16BE $ x)

i16ToBS :: Int.Int16 -> BS.ByteString
i16ToBS
    | Endian.targetByteOrder == Endian.LittleEndian = i16ToBSle
    | otherwise = i16ToBSbe
i16FromBS :: BS.ByteString -> Int.Int16
i16FromBS
    | Endian.targetByteOrder == Endian.LittleEndian = i16FromBSle
    | otherwise = i16FromBSbe

-- Int.Int32
i32FromBSle :: BS.ByteString -> Int.Int32
i32FromBSle x = BG.runGet BG.getInt32le (BL.fromStrict x)

i32ToBSle :: Int.Int32 -> BS.ByteString
i32ToBSle x = BL.toStrict (BB.toLazyByteString . BB.int32LE $ x)

i32FromBSbe :: BS.ByteString -> Int.Int32
i32FromBSbe x = BG.runGet BG.getInt32be (BL.fromStrict x)

i32ToBSbe :: Int.Int32 -> BS.ByteString
i32ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.int32BE $ x)

i32ToBS :: Int.Int32 -> BS.ByteString
i32ToBS
    | Endian.targetByteOrder == Endian.LittleEndian = i32ToBSle
    | otherwise = i32ToBSbe
i32FromBS :: BS.ByteString -> Int.Int32
i32FromBS
    | Endian.targetByteOrder == Endian.LittleEndian = i32FromBSle
    | otherwise = i32FromBSbe

-- Int.Int64
i64FromBSle :: BS.ByteString -> Int.Int64
i64FromBSle x = BG.runGet BG.getInt64le (BL.fromStrict x)

i64ToBSle :: Int.Int64 -> BS.ByteString
i64ToBSle x = BL.toStrict (BB.toLazyByteString . BB.int64LE $ x)

i64FromBSbe :: BS.ByteString -> Int.Int64
i64FromBSbe x = BG.runGet BG.getInt64be (BL.fromStrict x)

i64ToBSbe :: Int.Int64 -> BS.ByteString
i64ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.int64BE $ x)

i64ToBS :: Int.Int64 -> BS.ByteString
i64ToBS
    | Endian.targetByteOrder == Endian.LittleEndian = i64ToBSle
    | otherwise = i64ToBSbe

i64FromBS :: BS.ByteString -> Int.Int64
i64FromBS
    | Endian.targetByteOrder == Endian.LittleEndian = i64FromBSle
    | otherwise = i64FromBSbe

-- UInt.Word8
u8FromBS :: BS.ByteString -> UInt.Word8
u8FromBS x = BG.runGet BG.getWord8 (BL.fromStrict x)

u8ToBS :: UInt.Word8 -> BS.ByteString
u8ToBS x = BL.toStrict (BB.toLazyByteString . BB.word8 $ x)

-- UInt.Word16
u16FromBSle :: BS.ByteString -> UInt.Word16
u16FromBSle x = BG.runGet BG.getWord16le (BL.fromStrict x)

u16ToBSle :: UInt.Word16 -> BS.ByteString
u16ToBSle x = BL.toStrict (BB.toLazyByteString . BB.word16LE $ x)

u16FromBSbe :: BS.ByteString -> UInt.Word16
u16FromBSbe x = BG.runGet BG.getWord16be (BL.fromStrict x)

u16ToBSbe :: UInt.Word16 -> BS.ByteString
u16ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.word16BE $ x)

u16ToBS :: UInt.Word16 -> BS.ByteString
u16ToBS
    | Endian.targetByteOrder == Endian.LittleEndian = u16ToBSle
    | otherwise = u16ToBSbe
u16FromBS :: BS.ByteString -> UInt.Word16
u16FromBS
    | Endian.targetByteOrder == Endian.LittleEndian = u16FromBSle
    | otherwise = u16FromBSbe

-- UInt.Word32
u32FromBSle :: BS.ByteString -> UInt.Word32
u32FromBSle x = BG.runGet BG.getWord32le (BL.fromStrict x)

u32ToBSle :: UInt.Word32 -> BS.ByteString
u32ToBSle x = BL.toStrict (BB.toLazyByteString . BB.word32LE $ x)

u32FromBSbe :: BS.ByteString -> UInt.Word32
u32FromBSbe x = BG.runGet BG.getWord32be (BL.fromStrict x)

u32ToBSbe :: UInt.Word32 -> BS.ByteString
u32ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.word32BE $ x)

u32ToBS :: UInt.Word32 -> BS.ByteString
u32ToBS
    | Endian.targetByteOrder == Endian.LittleEndian = u32ToBSle
    | otherwise = u32ToBSbe
u32FromBS :: BS.ByteString -> UInt.Word32
u32FromBS
    | Endian.targetByteOrder == Endian.LittleEndian = u32FromBSle
    | otherwise = u32FromBSbe

-- UInt.Word64
u64FromBSle :: BS.ByteString -> UInt.Word64
u64FromBSle x = BG.runGet BG.getWord64le (BL.fromStrict x)

u64ToBSle :: UInt.Word64 -> BS.ByteString
u64ToBSle x = BL.toStrict (BB.toLazyByteString . BB.word64LE $ x)

u64FromBSbe :: BS.ByteString -> UInt.Word64
u64FromBSbe x = BG.runGet BG.getWord64be (BL.fromStrict x)

u64ToBSbe :: UInt.Word64 -> BS.ByteString
u64ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.word64BE $ x)

u64ToBS :: UInt.Word64 -> BS.ByteString
u64ToBS
    | Endian.targetByteOrder == Endian.LittleEndian = u64ToBSle
    | otherwise = u64ToBSbe
u64FromBS :: BS.ByteString -> UInt.Word64
u64FromBS
    | Endian.targetByteOrder == Endian.LittleEndian = u64FromBSle
    | otherwise = u64FromBSbe
