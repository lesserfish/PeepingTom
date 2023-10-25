module PeepingTom.Type (
    Type (..),
    sizeOf,
    intTypes,
) where

import PeepingTom.Internal

data Type
    = Void
    | Int8
    | Int16
    | Int32
    | Int64
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | Flt
    | Dbl
    | Bytes Int
    deriving (Show, Eq)

sizeOf :: Type -> Size
sizeOf Void = 0
sizeOf Int8 = 1
sizeOf Int16 = 2
sizeOf Int32 = 4
sizeOf Int64 = 8
sizeOf UInt8 = 1
sizeOf UInt16 = 2
sizeOf UInt32 = 4
sizeOf UInt64 = 8
sizeOf Flt = 4
sizeOf Dbl = 8
sizeOf (Bytes count) = count

intTypes :: [Type]
intTypes = [Int8, Int16, Int32, Int64]
