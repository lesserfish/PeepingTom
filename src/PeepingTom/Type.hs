{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PeepingTom.Type (
    Type (..),
    TypeClass,
    Void (..),
    Int8 (..),
    Int16 (..),
    Int32 (..),
    Int64 (..),
    UInt8 (..),
    UInt16 (..),
    UInt32 (..),
    UInt64 (..),
    Flt (..),
    Dbl (..),
    sizeOf,
) where

import qualified Data.Binary.Get as BG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Typeable
import qualified GHC.ByteOrder as Endian
import PeepingTom.Internal

class (Typeable a, Show a) => TypeClass a where
    sizeof :: a -> Size

data Type = forall a. (TypeClass a) => Type a

instance Show Type where
    show (Type x) = show x

data Void = Void deriving (Show, Typeable)
data Int8 = Int8 deriving (Show, Typeable)
data Int16 = Int16 deriving (Show, Typeable)
data Int32 = Int32 deriving (Show, Typeable)
data Int64 = Int64 deriving (Show, Typeable)
data UInt8 = UInt8 deriving (Show, Typeable)
data UInt16 = UInt16 deriving (Show, Typeable)
data UInt32 = UInt32 deriving (Show, Typeable)
data UInt64 = UInt64 deriving (Show, Typeable)
data Flt = Flt deriving (Show, Typeable)
data Dbl = Dbl deriving (Show, Typeable)

{-# INLINE sizeOf #-}
sizeOf :: Type -> Size
sizeOf (Type x) = sizeof x

instance TypeClass Void where
    sizeof _ = 0
instance TypeClass Int8 where
    {-# INLINE sizeof #-}
    sizeof _ = 1
instance TypeClass Int16 where
    {-# INLINE sizeof #-}
    sizeof _ = 2
instance TypeClass Int32 where
    {-# INLINE sizeof #-}
    sizeof _ = 4
instance TypeClass Int64 where
    {-# INLINE sizeof #-}
    sizeof _ = 8
instance TypeClass UInt8 where
    {-# INLINE sizeof #-}
    sizeof _ = 1
instance TypeClass UInt16 where
    {-# INLINE sizeof #-}
    sizeof _ = 2
instance TypeClass UInt32 where
    {-# INLINE sizeof #-}
    sizeof _ = 4
instance TypeClass UInt64 where
    {-# INLINE sizeof #-}
    sizeof _ = 8
instance TypeClass Flt where
    {-# INLINE sizeof #-}
    sizeof _ = 4
instance TypeClass Dbl where
    {-# INLINE sizeof #-}
    sizeof _ = 4
