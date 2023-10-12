{-# LANGUAGE TypeSynonymInstances #-}

module Type where

import qualified Data.Binary.Get as BG
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Int as I
import Data.Typeable
import qualified Data.Word as W
import qualified GHC.ByteOrder as Endian

class (Typeable a, Show a) => Type a where
    name :: a -> String
    size :: a -> Int
    fromByteString :: B.ByteString -> a
    toByteString :: a -> B.ByteString

-- Print the value with additional type info
showT :: (Type a) => a -> String
showT t = (name t) ++ ":" ++ show t

helperT :: (Type a) => a -> (b -> a) -> b -> a
helperT _ f x = f x

-- Helper function. Allows you to call fromByteString without necessarily knowing the underlying type of the variable.
updateT :: (Type a) => a -> B.ByteString -> a
updateT a bs = helperT a (\x -> fromByteString x) bs

type Int8 = I.Int8
type Int16 = I.Int16
type Int32 = I.Int32
type Int64 = I.Int64

type UInt8 = W.Word8
type UInt16 = W.Word16
type UInt32 = W.Word32
type UInt64 = W.Word64

-- Int8
i8FromBS :: B.ByteString -> Int8
i8FromBS x = BG.runGet BG.getInt8 (BL.fromStrict x)

i8ToBS :: Int8 -> B.ByteString
i8ToBS x = BL.toStrict (BB.toLazyByteString . BB.int8 $ x)

instance Type Int8 where
    name _ = "I8"
    size _ = 1
    fromByteString = i8FromBS
    toByteString = i8ToBS

-- Int16
i16FromBSle :: B.ByteString -> Int16
i16FromBSle x = BG.runGet BG.getInt16le (BL.fromStrict x)

i16ToBSle :: Int16 -> B.ByteString
i16ToBSle x = BL.toStrict (BB.toLazyByteString . BB.int16LE $ x)

i16FromBSbe :: B.ByteString -> Int16
i16FromBSbe x = BG.runGet BG.getInt16be (BL.fromStrict x)

i16ToBSbe :: Int16 -> B.ByteString
i16ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.int16BE $ x)
instance Type Int16 where
    name _ = "I16"
    size _ = 2
    fromByteString
        | Endian.targetByteOrder == Endian.LittleEndian = i16FromBSle
        | otherwise = i16FromBSbe
    toByteString
        | Endian.targetByteOrder == Endian.LittleEndian = i16ToBSle
        | otherwise = i16ToBSbe

-- Int32
i32FromBSle :: B.ByteString -> Int32
i32FromBSle x = BG.runGet BG.getInt32le (BL.fromStrict x)

i32ToBSle :: Int32 -> B.ByteString
i32ToBSle x = BL.toStrict (BB.toLazyByteString . BB.int32LE $ x)

i32FromBSbe :: B.ByteString -> Int32
i32FromBSbe x = BG.runGet BG.getInt32be (BL.fromStrict x)

i32ToBSbe :: Int32 -> B.ByteString
i32ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.int32BE $ x)
instance Type Int32 where
    name _ = "I32"
    size _ = 4
    fromByteString
        | Endian.targetByteOrder == Endian.LittleEndian = i32FromBSle
        | otherwise = i32FromBSbe
    toByteString
        | Endian.targetByteOrder == Endian.LittleEndian = i32ToBSle
        | otherwise = i32ToBSbe

-- Int64
i64FromBSle :: B.ByteString -> Int64
i64FromBSle x = BG.runGet BG.getInt64le (BL.fromStrict x)

i64ToBSle :: Int64 -> B.ByteString
i64ToBSle x = BL.toStrict (BB.toLazyByteString . BB.int64LE $ x)

i64FromBSbe :: B.ByteString -> Int64
i64FromBSbe x = BG.runGet BG.getInt64be (BL.fromStrict x)

i64ToBSbe :: Int64 -> B.ByteString
i64ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.int64BE $ x)
instance Type Int64 where
    name _ = "I64"
    size _ = 8
    fromByteString
        | Endian.targetByteOrder == Endian.LittleEndian = i64FromBSle
        | otherwise = i64FromBSbe
    toByteString
        | Endian.targetByteOrder == Endian.LittleEndian = i64ToBSle
        | otherwise = i64ToBSbe

-- Word8
u8FromBS :: B.ByteString -> UInt8
u8FromBS x = BG.runGet BG.getWord8 (BL.fromStrict x)

u8ToBS :: UInt8 -> B.ByteString
u8ToBS x = BL.toStrict (BB.toLazyByteString . BB.word8 $ x)

instance Type UInt8 where
    name _ = "U8"
    size _ = 1
    fromByteString = u8FromBS
    toByteString = u8ToBS

-- Word16
u16FromBSle :: B.ByteString -> UInt16
u16FromBSle x = BG.runGet BG.getWord16le (BL.fromStrict x)

u16ToBSle :: UInt16 -> B.ByteString
u16ToBSle x = BL.toStrict (BB.toLazyByteString . BB.word16LE $ x)

u16FromBSbe :: B.ByteString -> UInt16
u16FromBSbe x = BG.runGet BG.getWord16be (BL.fromStrict x)

u16ToBSbe :: UInt16 -> B.ByteString
u16ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.word16BE $ x)
instance Type UInt16 where
    name _ = "U16"
    size _ = 2
    fromByteString
        | Endian.targetByteOrder == Endian.LittleEndian = u16FromBSle
        | otherwise = u16FromBSbe
    toByteString
        | Endian.targetByteOrder == Endian.LittleEndian = u16ToBSle
        | otherwise = u16ToBSbe

-- Word32
u32FromBSle :: B.ByteString -> UInt32
u32FromBSle x = BG.runGet BG.getWord32le (BL.fromStrict x)

u32ToBSle :: UInt32 -> B.ByteString
u32ToBSle x = BL.toStrict (BB.toLazyByteString . BB.word32LE $ x)

u32FromBSbe :: B.ByteString -> UInt32
u32FromBSbe x = BG.runGet BG.getWord32be (BL.fromStrict x)

u32ToBSbe :: UInt32 -> B.ByteString
u32ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.word32BE $ x)
instance Type UInt32 where
    name _ = "U32"
    size _ = 4
    fromByteString
        | Endian.targetByteOrder == Endian.LittleEndian = u32FromBSle
        | otherwise = u32FromBSbe
    toByteString
        | Endian.targetByteOrder == Endian.LittleEndian = u32ToBSle
        | otherwise = u32ToBSbe

-- Word64
u64FromBSle :: B.ByteString -> UInt64
u64FromBSle x = BG.runGet BG.getWord64le (BL.fromStrict x)

u64ToBSle :: UInt64 -> B.ByteString
u64ToBSle x = BL.toStrict (BB.toLazyByteString . BB.word64LE $ x)

u64FromBSbe :: B.ByteString -> UInt64
u64FromBSbe x = BG.runGet BG.getWord64be (BL.fromStrict x)

u64ToBSbe :: UInt64 -> B.ByteString
u64ToBSbe x = BL.toStrict (BB.toLazyByteString . BB.word64BE $ x)
instance Type UInt64 where
    name _ = "U64"
    size _ = 8
    fromByteString
        | Endian.targetByteOrder == Endian.LittleEndian = u64FromBSle
        | otherwise = u64FromBSbe
    toByteString
        | Endian.targetByteOrder == Endian.LittleEndian = u64ToBSle
        | otherwise = u64ToBSbe

-- Float
fFromBSle :: B.ByteString -> Float
fFromBSle x = BG.runGet BG.getFloatle (BL.fromStrict x)

fToBSle :: Float -> B.ByteString
fToBSle x = BL.toStrict (BB.toLazyByteString . BB.floatLE $ x)

fFromBSbe :: B.ByteString -> Float
fFromBSbe x = BG.runGet BG.getFloatbe (BL.fromStrict x)

fToBSbe :: Float -> B.ByteString
fToBSbe x = BL.toStrict (BB.toLazyByteString . BB.floatBE $ x)

instance Type Float where
    name _ = "F"
    size _ = 4
    fromByteString
        | Endian.targetByteOrder == Endian.LittleEndian = fFromBSle
        | otherwise = fFromBSbe
    toByteString
        | Endian.targetByteOrder == Endian.LittleEndian = fToBSle
        | otherwise = fToBSbe

-- Double
dFromBSle :: B.ByteString -> Double
dFromBSle x = BG.runGet BG.getDoublele (BL.fromStrict x)

dToBSle :: Double -> B.ByteString
dToBSle x = BL.toStrict (BB.toLazyByteString . BB.doubleLE $ x)

dFromBSbe :: B.ByteString -> Double
dFromBSbe x = BG.runGet BG.getDoublebe (BL.fromStrict x)

dToBSbe :: Double -> B.ByteString
dToBSbe x = BL.toStrict (BB.toLazyByteString . BB.doubleBE $ x)

instance Type Double where
    name _ = "D"
    size _ = 8
    fromByteString
        | Endian.targetByteOrder == Endian.LittleEndian = dFromBSle
        | otherwise = dFromBSbe
    toByteString
        | Endian.targetByteOrder == Endian.LittleEndian = dToBSle
        | otherwise = dToBSbe
