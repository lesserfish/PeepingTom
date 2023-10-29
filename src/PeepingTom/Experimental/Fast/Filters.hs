{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module PeepingTom.Experimental.Fast.Filters where

import qualified Data.ByteString as BS
import qualified Data.Int as I
import Data.Maybe (catMaybes)
import Foreign.C
import Foreign.Ptr
import qualified PeepingTom.Conversions as Conv
import PeepingTom.Experimental.Fast.Common
import PeepingTom.Internal
import qualified PeepingTom.State as PT
import qualified PeepingTom.Type as T

data CFilter = CFilter
    { cfFPtr :: FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
    , cfMaxSize :: Int
    , cfReference :: BS.ByteString
    }

foreign import capi safe "C/Filters.c call"
    c_call ::
        FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt) -> -- Pointer to comparison function
        Ptr CChar -> -- Pointer to Memory chunk data
        Ptr CChar -> -- Pointer to reference value
        CSize -> -- sizeof reference value
        IO CUInt

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

i8Eq :: Integer -> CFilter
i8Eq value = CFilter c_i8eq 1 bsData
  where
    !bsData = Conv.i8ToBS (fromInteger value)

i16Eq :: Integer -> CFilter
i16Eq value = CFilter c_i16eq 2 bsData
  where
    !bsData = Conv.i16ToBS (fromInteger value)

i32Eq :: Integer -> CFilter
i32Eq value = CFilter c_i32eq 4 bsData
  where
    !bsData = Conv.i32ToBS (fromInteger value)

i64Eq :: Integer -> CFilter
i64Eq value = CFilter c_i64eq 8 bsData
  where
    !bsData = Conv.i64ToBS (fromInteger value)

i8Range :: Integer -> Bool
i8Range x = x >= fromIntegral (minBound :: I.Int8) && x <= fromIntegral (maxBound :: I.Int8)

i16Range :: Integer -> Bool
i16Range x = x >= fromIntegral (minBound :: I.Int16) && x <= fromIntegral (maxBound :: I.Int16)

i32Range :: Integer -> Bool
i32Range x = x >= fromIntegral (minBound :: I.Int32) && x <= fromIntegral (maxBound :: I.Int32)

i64Range :: Integer -> Bool
i64Range x = x >= fromIntegral (minBound :: I.Int64) && x <= fromIntegral (maxBound :: I.Int64)

pickIntFunPtr :: Integer -> FunPtr (Ptr CChar -> Ptr CChar -> CSize -> CUInt)
pickIntFunPtr value
    | i8Range value = c_inteq
    | i16Range value = c_int16peq
    | i32Range value = c_int32peq
    | i64Range value = c_i64eq
    | otherwise = c_void

eqInt :: Integer -> CFilter
eqInt value = CFilter func 8 bsData
  where
    !bsData = Conv.i64ToBS (fromInteger value)
    !func = pickIntFunPtr value

maxSizeOf :: [T.Type] -> Size
maxSizeOf types = output
  where
    output = foldr max 0 (map T.sizeOf types)

candidateFilter :: CFilter -> PT.Candidate -> IO (Maybe PT.Candidate)
candidateFilter cFltr candidate = do
    let bsData = PT.cData candidate
    let bsPtr = getBSPtr bsData
    let ref = cfReference cFltr
    let refPtr = getBSPtr ref
    let funPtr = cfFPtr cFltr
    let size = maxSizeOf (PT.cTypes candidate)
    encodedTypes <- c_call funPtr bsPtr refPtr (fromIntegral size)
    let types = decodeTypes encodedTypes size
    if length types == 0
        then return Nothing
        else do
            let new_size = maxSizeOf types
            let newBS = BS.take new_size bsData
            return $ Just candidate{PT.cTypes = types, PT.cData = newBS}

applyFilter :: CFilter -> PT.PeepState -> IO PT.PeepState
applyFilter fltr state = do
    candidates' <- mapM (candidateFilter fltr) (PT.psCandidates state) :: IO [Maybe PT.Candidate]
    let candidates = catMaybes candidates'
    let output = state{PT.psCandidates = candidates}
    return output
