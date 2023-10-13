{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Internal where

import Text.Printf

newtype PID = PID Int deriving (Show, Ord, Eq, Real, Enum, Integral, Num, PrintfArg)
newtype FD = FD Int deriving (Show, Ord, Eq, Real, Enum, Integral, Num, PrintfArg)
newtype Address = Address Int deriving (Show, Ord, Eq, Real, Enum, Integral, Num, PrintfArg)
newtype Size = Size Int deriving (Show, Ord, Eq, Real, Enum, Integral, Num, PrintfArg)
