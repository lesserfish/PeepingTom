{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PeepingTom.Fast.Filter (
    applyFilter,
    CFilter (..),
    eqInt,
    i8Eq,
    i16Eq,
    i32Eq,
    i64Eq,
    ltInt,
    i8Lt,
    i16Lt,
    i32Lt,
    i64Lt,
    gtInt,
    i8Gt,
    i16Gt,
    i32Gt,
    i64Gt,
    leqInt,
    i8Leq,
    i16Leq,
    i32Leq,
    i64Leq,
    geqInt,
    i8Geq,
    i16Geq,
    i32Geq,
    i64Geq,
    neqInt,
    i8Neq,
    i16Neq,
    i32Neq,
    i64Neq,
    eqBS,
    eqStr,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Int as I
import Data.Maybe (catMaybes)
import Foreign.C
import Foreign.Ptr
import PeepingTom.Conversions
import PeepingTom.Fast.Common
import PeepingTom.Internal
import PeepingTom.State
import PeepingTom.Type

data CFilter = CFilter
    { cfFPtr :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
    , cfMaxSize :: Int
    , cfReference :: BS.ByteString
    }

maxSizeOf :: [Type] -> Size
maxSizeOf types = output
  where
    output = foldr max 0 (map sizeOf types)

matchFilter :: CFilter -> Match -> IO (Maybe Match)
matchFilter cFltr match = do
    let bsData = mData match
    let bsPtr = getBSPtr bsData
    let ref = cfReference cFltr
    let refPtr = getBSPtr ref
    let funPtr = cfFPtr cFltr
    let size = maxSizeOf (mTypes match)
    encodedTypes <- c_call funPtr bsPtr refPtr (fromIntegral size)
    let types = decodeTypes encodedTypes size
    if length types == 0
        then return Nothing
        else do
            let new_size = maxSizeOf types
            let newBS = BS.take new_size bsData
            return $ Just match{mTypes = types, mData = newBS}

applyFilter :: CFilter -> PeepState -> IO PeepState
applyFilter fltr state = do
    matchs' <- mapM (matchFilter fltr) (psMatches state) :: IO [Maybe Match]
    let matchs = catMaybes matchs'
    let output = state{psMatches = matchs}
    return output

foreign import capi safe "C/Filters.c call"
    c_call ::
        FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt) -> -- Pointer to comparison function
        Ptr CChar -> -- Pointer to Memory chunk data
        Ptr CChar -> -- Pointer to reference value
        CSize -> -- sizeof reference value
        IO CUInt

-- EQ

foreign import capi safe "C/Filters.c &i8_eq"
    c_i8eq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i16_eq"
    c_i16eq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i32_eq"
    c_i32eq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i64_eq"
    c_i64eq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int_eq"
    c_inteq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int16p_eq"
    c_int16peq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int32p_eq"
    c_int32peq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &voidf"
    c_void :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

i8Eq :: I.Int8 -> CFilter
i8Eq value = CFilter c_i8eq 1 bsData
  where
    !bsData = i8ToBS value

i16Eq :: I.Int16 -> CFilter
i16Eq value = CFilter c_i16eq 2 bsData
  where
    !bsData = i16ToBS value

i32Eq :: I.Int32 -> CFilter
i32Eq value = CFilter c_i32eq 4 bsData
  where
    !bsData = i32ToBS value

i64Eq :: I.Int64 -> CFilter
i64Eq value = CFilter c_i64eq 8 bsData
  where
    !bsData = i64ToBS value

i8Range :: Integer -> Bool
i8Range x = x >= fromIntegral (minBound :: I.Int8) && x <= fromIntegral (maxBound :: I.Int8)

i16Range :: Integer -> Bool
i16Range x = x >= fromIntegral (minBound :: I.Int16) && x <= fromIntegral (maxBound :: I.Int16)

i32Range :: Integer -> Bool
i32Range x = x >= fromIntegral (minBound :: I.Int32) && x <= fromIntegral (maxBound :: I.Int32)

i64Range :: Integer -> Bool
i64Range x = x >= fromIntegral (minBound :: I.Int64) && x <= fromIntegral (maxBound :: I.Int64)

pickIntEqFunPtr :: Integer -> FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
pickIntEqFunPtr value
    | i8Range value = c_inteq
    | i16Range value = c_int16peq
    | i32Range value = c_int32peq
    | i64Range value = c_i64eq
    | otherwise = c_void

eqInt :: Integer -> CFilter
eqInt value = CFilter func 8 bsData
  where
    !bsData = i64ToBS (fromInteger value)
    !func = pickIntEqFunPtr value

-- LT

foreign import capi safe "C/Filters.c &i8_lt"
    c_i8lt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i16_lt"
    c_i16lt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i32_lt"
    c_i32lt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i64_lt"
    c_i64lt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int_lt"
    c_intlt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int16p_lt"
    c_int16plt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int32p_lt"
    c_int32plt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

i8Lt :: I.Int8 -> CFilter
i8Lt value = CFilter c_i8lt 1 bsData
  where
    !bsData = i8ToBS value

i16Lt :: I.Int16 -> CFilter
i16Lt value = CFilter c_i16lt 2 bsData
  where
    !bsData = i16ToBS value

i32Lt :: I.Int32 -> CFilter
i32Lt value = CFilter c_i32lt 4 bsData
  where
    !bsData = i32ToBS value

i64Lt :: I.Int64 -> CFilter
i64Lt value = CFilter c_i64lt 8 bsData
  where
    !bsData = i64ToBS value

pickIntLtFunPtr :: Integer -> FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
pickIntLtFunPtr value
    | i8Range value = c_intlt
    | i16Range value = c_int16plt
    | i32Range value = c_int32plt
    | i64Range value = c_i64lt
    | otherwise = c_void

ltInt :: Integer -> CFilter
ltInt value = CFilter func 8 bsData
  where
    !bsData = i64ToBS (fromInteger value)
    !func = pickIntLtFunPtr value

-- LEQ

foreign import capi safe "C/Filters.c &i8_leq"
    c_i8leq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i16_leq"
    c_i16leq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i32_leq"
    c_i32leq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i64_leq"
    c_i64leq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int_leq"
    c_intleq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int16p_leq"
    c_int16pleq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int32p_leq"
    c_int32pleq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

i8Leq :: I.Int8 -> CFilter
i8Leq value = CFilter c_i8leq 1 bsData
  where
    !bsData = i8ToBS value

i16Leq :: I.Int16 -> CFilter
i16Leq value = CFilter c_i16leq 2 bsData
  where
    !bsData = i16ToBS value

i32Leq :: I.Int32 -> CFilter
i32Leq value = CFilter c_i32leq 4 bsData
  where
    !bsData = i32ToBS value

i64Leq :: I.Int64 -> CFilter
i64Leq value = CFilter c_i64leq 8 bsData
  where
    !bsData = i64ToBS value

pickIntLeqFunPtr :: Integer -> FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
pickIntLeqFunPtr value
    | i8Range value = c_intleq
    | i16Range value = c_int16pleq
    | i32Range value = c_int32pleq
    | i64Range value = c_i64leq
    | otherwise = c_void

leqInt :: Integer -> CFilter
leqInt value = CFilter func 8 bsData
  where
    !bsData = i64ToBS (fromInteger value)
    !func = pickIntLeqFunPtr value

-- GT

foreign import capi safe "C/Filters.c &i8_gt"
    c_i8gt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i16_gt"
    c_i16gt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i32_gt"
    c_i32gt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i64_gt"
    c_i64gt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int_gt"
    c_intgt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int16p_gt"
    c_int16pgt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int32p_gt"
    c_int32pgt :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

i8Gt :: I.Int8 -> CFilter
i8Gt value = CFilter c_i8gt 1 bsData
  where
    !bsData = i8ToBS value

i16Gt :: I.Int16 -> CFilter
i16Gt value = CFilter c_i16gt 2 bsData
  where
    !bsData = i16ToBS value

i32Gt :: I.Int32 -> CFilter
i32Gt value = CFilter c_i32gt 4 bsData
  where
    !bsData = i32ToBS value

i64Gt :: I.Int64 -> CFilter
i64Gt value = CFilter c_i64gt 8 bsData
  where
    !bsData = i64ToBS value

pickIntGtFunPtr :: Integer -> FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
pickIntGtFunPtr value
    | i8Range value = c_intgt
    | i16Range value = c_int16pgt
    | i32Range value = c_int32pgt
    | i64Range value = c_i64gt
    | otherwise = c_void

gtInt :: Integer -> CFilter
gtInt value = CFilter func 8 bsData
  where
    !bsData = i64ToBS (fromInteger value)
    !func = pickIntGtFunPtr value

-- Geq

foreign import capi safe "C/Filters.c &i8_geq"
    c_i8geq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i16_geq"
    c_i16geq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i32_geq"
    c_i32geq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i64_geq"
    c_i64geq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int_geq"
    c_intgeq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int16p_geq"
    c_int16pgeq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int32p_geq"
    c_int32pgeq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

i8Geq :: I.Int8 -> CFilter
i8Geq value = CFilter c_i8geq 1 bsData
  where
    !bsData = i8ToBS value

i16Geq :: I.Int16 -> CFilter
i16Geq value = CFilter c_i16geq 2 bsData
  where
    !bsData = i16ToBS value

i32Geq :: I.Int32 -> CFilter
i32Geq value = CFilter c_i32geq 4 bsData
  where
    !bsData = i32ToBS value

i64Geq :: I.Int64 -> CFilter
i64Geq value = CFilter c_i64geq 8 bsData
  where
    !bsData = i64ToBS value

pickIntGeqFunPtr :: Integer -> FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
pickIntGeqFunPtr value
    | i8Range value = c_intgeq
    | i16Range value = c_int16pgeq
    | i32Range value = c_int32pgeq
    | i64Range value = c_i64geq
    | otherwise = c_void

geqInt :: Integer -> CFilter
geqInt value = CFilter func 8 bsData
  where
    !bsData = i64ToBS (fromInteger value)
    !func = pickIntGeqFunPtr value

-- NEQ

foreign import capi safe "C/Filters.c &i8_neq"
    c_i8neq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i16_neq"
    c_i16neq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i32_neq"
    c_i32neq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &i64_neq"
    c_i64neq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int_neq"
    c_intneq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int16p_neq"
    c_int16pneq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

foreign import capi safe "C/Filters.c &int32p_neq"
    c_int32pneq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

i8Neq :: I.Int8 -> CFilter
i8Neq value = CFilter c_i8neq 1 bsData
  where
    !bsData = i8ToBS value

i16Neq :: I.Int16 -> CFilter
i16Neq value = CFilter c_i16neq 2 bsData
  where
    !bsData = i16ToBS value

i32Neq :: I.Int32 -> CFilter
i32Neq value = CFilter c_i32neq 4 bsData
  where
    !bsData = i32ToBS value

i64Neq :: I.Int64 -> CFilter
i64Neq value = CFilter c_i64neq 8 bsData
  where
    !bsData = i64ToBS value

pickIntNeqFunPtr :: Integer -> FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
pickIntNeqFunPtr value
    | i8Range value = c_intneq
    | i16Range value = c_int16pneq
    | i32Range value = c_int32pneq
    | i64Range value = c_i64neq
    | otherwise = c_void

neqInt :: Integer -> CFilter
neqInt value = CFilter func 8 bsData
  where
    !bsData = i64ToBS (fromInteger value)
    !func = pickIntNeqFunPtr value

-- ByteString

foreign import capi safe "C/Filters.c &bs_eq"
    c_bseq :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)

eqBS :: BS.ByteString -> CFilter
eqBS bs = CFilter c_bseq (BS.length bs) bs

eqStr :: String -> CFilter
eqStr str = eqBS bs
  where
    !bs = BSC.pack str
