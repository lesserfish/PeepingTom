module PeepingTom.Filters where

import qualified Data.ByteString as BS
import Data.Functor ((<$>))
import Data.List (any)
import Data.Maybe (Maybe, fromMaybe)
import Data.Typeable (Typeable, typeOf)
import PeepingTom.Conversions
import PeepingTom.Type

isType :: (Typeable a) => a -> Type -> Bool
isType a (Type b) = typeOf a == typeOf b

isI8 :: Type -> Bool
isI8 = isType Int8

isI16 :: Type -> Bool
isI16 = isType Int16

isI32 :: Type -> Bool
isI32 = isType Int32

isI64 :: Type -> Bool
isI64 = isType Int64

isU8 :: Type -> Bool
isU8 = isType UInt8

isU16 :: Type -> Bool
isU16 = isType UInt16

isU32 :: Type -> Bool
isU32 = isType UInt32

isU64 :: Type -> Bool
isU64 = isType UInt64

isFlt :: Type -> Bool
isFlt = isType Flt

isDbl :: Type -> Bool
isDbl = isType Dbl

asType :: Type -> BS.ByteString -> a -> Maybe a
asType t bs x =
    if (BS.length bs < sizeOf t)
        then Nothing
        else Just $ x

castInteger' :: Type -> BS.ByteString -> Maybe Integer
castInteger' t b
    | isI8 t = Just . toInteger $ i8FromBS b
    | isI16 t = Just . toInteger $ i16FromBS b
    | isI32 t = Just . toInteger $ i32FromBS b
    | isI64 t = Just . toInteger $ i64FromBS b
    | isU8 t = Just . toInteger $ u8FromBS b
    | isU16 t = Just . toInteger $ u16FromBS b
    | isU32 t = Just . toInteger $ u32FromBS b
    | isU64 t = Just . toInteger $ u64FromBS b
    | otherwise = Nothing

castInteger :: Type -> BS.ByteString -> Maybe Integer
castInteger t bs = (castInteger' t bs) >>= (asType t bs)

compareInteger' :: (Integer -> Bool) -> BS.ByteString -> Type -> Maybe Bool
compareInteger' fltr bs t = fltr <$> (castInteger t bs)

compareInteger :: (Integer -> Bool) -> Filter
compareInteger fltr bs t = fromMaybe False (compareInteger' fltr bs t)

-- ByteString
compareBS :: BS.ByteString -> Filter
compareBS bs1 bs2 _ = bs1 == bs2

-- TypeData: Comparisons with a given Type and a ByteString
data TypeData = TypeData {aType :: Type, aData :: BS.ByteString}

compareTD :: TypeData -> Filter
compareTD td bs t = (typeOf (aType td) == typeOf t) && (aData td == (BS.take (sizeOf t) bs))

compareTD' :: BS.ByteString -> Type -> TypeData -> Bool
compareTD' bs t td = compareTD td bs t

compareTDA :: [TypeData] -> Filter
compareTDA tda bs t = any (compareTD' bs t) tda

integerTD :: Type -> Integer -> TypeData
integerTD t i
    | isI8 t = TypeData t (i8ToBS . fromIntegral $ i)
    | isI16 t = TypeData t (i16ToBS . fromIntegral $ i)
    | isI32 t = TypeData t (i32ToBS . fromIntegral $ i)
    | isI64 t = TypeData t (i64ToBS . fromIntegral $ i)
    | isU8 t = TypeData t (u8ToBS . fromIntegral $ i)
    | isU16 t = TypeData t (u16ToBS . fromIntegral $ i)
    | isU32 t = TypeData t (u32ToBS . fromIntegral $ i)
    | isU64 t = TypeData t (u64ToBS . fromIntegral $ i)
    | otherwise = TypeData t (BS.empty)

integerTDA :: Integer -> [TypeData]
integerTDA i =
    [ integerTD (Type Int8) (fromIntegral i)
    , integerTD (Type Int16) (fromIntegral i)
    , integerTD (Type Int32) (fromIntegral i)
    , integerTD (Type Int64) (fromIntegral i)
    ]

eqInteger :: Integer -> Filter
eqInteger i = compareTDA (integerTDA i)
