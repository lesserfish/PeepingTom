module TypeFilter where

import Data.Typeable
import Type

castInteger' :: (Integral a) => Maybe a -> Maybe Integer
castInteger' Nothing = Nothing
castInteger' (Just num) = Just (Prelude.toInteger num)

castInteger :: (Type a) => a -> Maybe Integer
castInteger value
    | typeOf value == typeOf (0 :: Int8) = castInteger' (cast value :: Maybe Int8)
    | typeOf value == typeOf (0 :: Int16) = castInteger' (cast value :: Maybe Int16)
    | typeOf value == typeOf (0 :: Int32) = castInteger' (cast value :: Maybe Int32)
    | typeOf value == typeOf (0 :: Int64) = castInteger' (cast value :: Maybe Int64)
    | otherwise = Nothing

compareInt :: (Type a) => (Integer -> Bool) -> a -> Bool
compareInt comparison v = output
  where
    x = castInteger v
    output = case x of
        Nothing -> False
        Just i -> comparison i

type Filter a = a -> Maybe Bool

func :: (Type a) => Filter a
func = undefined
