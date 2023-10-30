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

writeCandidate :: IO.WInterface -> Writer -> Candidate -> IO Candidate
writeCandidate winterface writer candidate = do
    let addr = cAddress candidate
    let maxtype = maxType . cTypes $ candidate -- Get the type with the largest size
    let bs_data = writer maxtype -- Get the bytes to write
    let data_size = BS.length bs_data
    if data_size == 0
        then return candidate -- Writer does not support this type; Don't write anything
        else do
            winterface addr bs_data
            let candidate' = candidate{cData = bs_data}
            return candidate'

applyWriterHelper :: IO.WInterface -> Writer -> [Candidate] -> IO [Candidate]
applyWriterHelper winterface writer candidates = sequence $ map (writeCandidate winterface writer) candidates

applyWriter :: Writer -> PeepState -> IO PeepState
applyWriter = applyWriterS defaultScanOptions

applyWriterS :: ScanOptions -> Writer -> PeepState -> IO PeepState
applyWriterS scopt writer peepstate = do
    let stopsig = soSIGSTOP scopt
    let pid = psPID peepstate
    let candidates = psCandidates peepstate
    let action = (\winterface -> applyWriterHelper winterface writer candidates) :: (IO.WInterface -> IO [Candidate])
    candidates' <- IO.withWInterface pid stopsig action
    return $ peepstate{psCandidates = candidates'}
