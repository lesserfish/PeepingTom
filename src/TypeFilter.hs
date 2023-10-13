module TypeFilter where

import qualified Data.ByteString as B
import Data.Typeable
import Type

-- Filters to compare to a specific Integer Type
castInteger' :: (Integral a) => Maybe a -> Maybe Integer
castInteger' Nothing = Nothing
castInteger' (Just num) = Just (Prelude.toInteger num)

castInteger :: (Type a) => a -> Maybe Integer
castInteger value
    | typeOf value == typeOf (0 :: Int8) = castInteger' (cast value :: Maybe Int8)
    | typeOf value == typeOf (0 :: Int16) = castInteger' (cast value :: Maybe Int16)
    | typeOf value == typeOf (0 :: Int32) = castInteger' (cast value :: Maybe Int32)
    | typeOf value == typeOf (0 :: Int64) = castInteger' (cast value :: Maybe Int64)
    | typeOf value == typeOf (0 :: UInt8) = castInteger' (cast value :: Maybe UInt8)
    | typeOf value == typeOf (0 :: UInt16) = castInteger' (cast value :: Maybe UInt16)
    | typeOf value == typeOf (0 :: UInt32) = castInteger' (cast value :: Maybe UInt32)
    | typeOf value == typeOf (0 :: UInt64) = castInteger' (cast value :: Maybe UInt64)
    | otherwise = Nothing

compareInt :: (Type a) => (Integer -> Bool) -> a -> Bool
compareInt comparison v = output
  where
    x = castInteger v
    output = case x of
        Nothing -> False
        Just i -> comparison i

-- Filters to compare to a specific Rational Type

castRational' :: (RealFloat a) => Maybe a -> Maybe Double
castRational' Nothing = Nothing
castRational' (Just num) = Just (realToFrac num)

castRational :: (Type a) => a -> Maybe Double
castRational value
    | typeOf value == typeOf (0 :: Float) = castRational' (cast value :: Maybe Float)
    | typeOf value == typeOf (0 :: Double) = castRational' (cast value :: Maybe Double)
    | otherwise = Nothing

compareFloat :: (Type a) => (Double -> Bool) -> a -> Bool
compareFloat comparison v = output
  where
    x = castRational v
    output = case x of
        Nothing -> False
        Just r -> comparison r

-- Filters to select a specific Type
isType :: (Type a, Type b) => a -> b -> Bool
isType x y
    | typeOf (x) == typeOf (y) = True
    | otherwise = False

isInt8 :: (Type a) => a -> Bool
isInt8 = isType (0 :: Int8)

isInt16 :: (Type a) => a -> Bool
isInt16 = isType (0 :: Int16)

isInt32 :: (Type a) => a -> Bool
isInt32 = isType (0 :: Int32)

isInt64 :: (Type a) => a -> Bool
isInt64 = isType (0 :: Int64)

isUInt8 :: (Type a) => a -> Bool
isUInt8 = isType (0 :: UInt8)

isUInt16 :: (Type a) => a -> Bool
isUInt16 = isType (0 :: UInt16)

isUInt32 :: (Type a) => a -> Bool
isUInt32 = isType (0 :: UInt32)

isUInt64 :: (Type a) => a -> Bool
isUInt64 = isType (0 :: UInt64)

isFloat :: (Type a) => a -> Bool
isFloat = isType (0 :: Float)

isDouble :: (Type a) => a -> Bool
isDouble = isType (0 :: Double)

-- Filters to compare ByteString

compareByteString :: (Type a) => B.ByteString -> a -> Bool
compareByteString bs a = (toByteString a) == bs

compareFromByteString :: (Type a) => B.ByteString -> a -> Bool
compareFromByteString bs a = a == (fromByteString bs)

-- Todo: Some additional filters about sub-bytestring
