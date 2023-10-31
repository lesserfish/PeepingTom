module PeepingTom.Writer (
    Writer,
    writeInt,
    writeBytes,
    writeBytes_,
    writeStr,
    writeStr_,
    applyWriter,
    applyWriterS,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import PeepingTom.Common
import qualified PeepingTom.Conversions as Conversions
import qualified PeepingTom.IO as IO
import PeepingTom.Scan
import PeepingTom.State
import PeepingTom.Type

type Writer = Type -> BS.ByteString

writeInt :: Integer -> Writer
writeInt i Int8 = Conversions.i8ToBS (fromIntegral i)
writeInt i Int16 = Conversions.i16ToBS (fromIntegral i)
writeInt i Int32 = Conversions.i32ToBS (fromIntegral i)
writeInt i Int64 = Conversions.i64ToBS (fromIntegral i)
writeInt i UInt8 = Conversions.u8ToBS (fromIntegral i)
writeInt i UInt16 = Conversions.u16ToBS (fromIntegral i)
writeInt i UInt32 = Conversions.u32ToBS (fromIntegral i)
writeInt i UInt64 = Conversions.u64ToBS (fromIntegral i)
writeInt _ _ = BS.empty

writeBytes :: BS.ByteString -> Writer
writeBytes bs (Bytes n)
    | BS.length bs == n = bs
    | otherwise = BS.empty
writeBytes _ _ = BS.empty

writeBytes_ :: BS.ByteString -> Writer
writeBytes_ bs (Bytes n) = bs
writeBytes_ _ _ = BS.empty

writeStr :: String -> Writer
writeStr str = writeBytes (BSC.pack str)

writeStr_ :: String -> Writer
writeStr_ str = writeBytes_ (BSC.pack str)

writeMatch :: IO.WInterface -> Writer -> Match -> IO Match
writeMatch winterface writer match = do
    let addr = mAddress match
    let maxtype = maxType . mTypes $ match -- Get the type with the largest size
    let bs_data = writer maxtype -- Get the bytes to write
    let data_size = BS.length bs_data
    if data_size == 0
        then return match -- Writer does not support this type; Don't write anything
        else do
            winterface addr bs_data
            let match' = match{mData = bs_data}
            return match'

applyWriterHelper :: IO.WInterface -> Writer -> [Match] -> IO [Match]
applyWriterHelper winterface writer matchs = sequence $ map (writeMatch winterface writer) matchs

applyWriter :: Writer -> PeepState -> IO PeepState
applyWriter = applyWriterS defaultScanOptions

applyWriterS :: ScanOptions -> Writer -> PeepState -> IO PeepState
applyWriterS scopt writer peepstate = do
    let stopsig = soSIGSTOP scopt
    let pid = psPID peepstate
    let matchs = psMatches peepstate
    let action = (\winterface -> applyWriterHelper winterface writer matchs) :: (IO.WInterface -> IO [Match])
    matchs' <- IO.withWInterface pid stopsig action
    return $ peepstate{psMatches = matchs'}
