module VMIO where

import qualified Data.ByteString as B
import qualified Maps as M
import qualified Posix as P
import Text.Printf

data RData = RData {regdata :: B.ByteString, reginfo :: M.Region}

instance Show RData where
    show rdata =
        show (reginfo rdata)
            ++ " \tBytes Read: "
            ++ show (B.length (regdata rdata))

memFile :: Int -> String
memFile pid = printf "/proc/%d/mem" pid

readChunks :: P.FD -> (Int, Int) -> Int -> B.ByteString -> IO B.ByteString
readChunks fd (start_address, end_address) chunk_size bs
    | start_address >= end_address = return bs
    | otherwise = do
        let count = min (end_address - start_address) chunk_size
        chunk <- P.pread fd count start_address
        let bs' = B.append bs chunk
        readChunks fd (start_address + chunk_size, end_address) chunk_size bs'

readFileSection :: FilePath -> (Int, Int) -> Int -> IO B.ByteString
readFileSection filepath (start_address, end_address) chunk_size = do
    fd <- P.open filepath P.O_RDONLY
    bytes <- readChunks fd (start_address, end_address) chunk_size B.empty
    _ <- P.close fd
    return bytes

loadRegion :: Int -> M.Region -> IO RData
loadRegion chunk_size region = do
    let pid = M.rPID region
    let filepath = memFile pid
    let start_address = M.startAddress region
    let end_address = M.endAddress region
    bytes <- readFileSection filepath (start_address, end_address) chunk_size
    let rdata = RData bytes region :: RData
    putStrLn $ "Finished loading region " ++ show (M.regionID region)
    return rdata

loadMap :: Int -> M.Regions -> IO [RData]
loadMap chunk_size m = sequence $ fmap (loadRegion chunk_size) m

debug :: IO ()
debug = do
    d <- readFileSection "/home/lesserfish/Documents/tmp/numbers.txt" (0, 8) 7
    putStrLn $ show d
    putStrLn $ show (B.length d)
    return ()
