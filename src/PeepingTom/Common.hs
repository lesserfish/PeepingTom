module PeepingTom.Common (
    bsToString,
    PeepingTom.Common.showList,
    slice,
    vocal,
    maxSizeOf,
    maxType,
) where

import qualified Data.ByteString as BS
import Data.List (intersperse)
import PeepingTom.Internal
import PeepingTom.Type
import Text.Printf (printf)

bsToString :: BS.ByteString -> String
bsToString bs = concat $ intersperse " " (fmap (printf "0x%02X") (BS.unpack bs))

showList :: (Show a) => [a] -> String
showList lst = concat $ intersperse "\n" (fmap show lst)

slice :: Address -> Address -> BS.ByteString -> BS.ByteString
slice start len = BS.take len . BS.drop start

vocal :: (a -> b -> IO ()) -> (a -> IO b) -> a -> (IO b)
vocal log action input = do
    output <- action input
    log input output
    return output

maxSizeOf :: [Type] -> Size
maxSizeOf types = output
  where
    output = foldr max 0 (map sizeOf types)

maxType :: [Type] -> Type
maxType [] = Void
maxType (t : rest)
    | sizeOf t < sizeOf (maxType rest) = (maxType rest)
    | otherwise = t
