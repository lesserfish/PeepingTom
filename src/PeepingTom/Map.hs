module PeepingTom.Map (
    Permission (..),
    MapID (..),
    Region (..),
    MapInfo (..),
    extractRegions,
    filterRegions,
    rFilterRW,
    rFilterR,
    rFilterW,
    rFilterMappings,
    defaultRFilter,
    totalBytes,
) where

import qualified Data.List.Split as SPL
import Numeric (readHex)
import PeepingTom.Internal
import qualified PeepingTom.Posix as Posix
import qualified System.IO as IO
import Text.Printf (printf)

data Permission = Permission {r :: Bool, w :: Bool, x :: Bool, s :: Bool}
data MapID = MapID {majorID :: Int, minorID :: Int, inodeID :: Int}
data Region = Region {rStartAddr :: Address, rEndAddr :: Address, rPermission :: Permission, rOffset :: Address, rMapID :: MapID, rFP :: FilePath, rID :: Int, rPID :: PID}
data MapInfo = MapInfo {miRegions :: [Region], miPID :: PID, miExecutableName :: String}

showRegions :: [Region] -> String
showRegions [] = ""
showRegions (y : ys) = show y ++ "\n" ++ (showRegions ys)

instance Show Permission where
    show p = sr ++ sw ++ sx ++ ss
      where
        sr = if (r p) then "r" else "-"
        sw = if (w p) then "w" else "-"
        sx = if (x p) then "x" else "-"
        ss = if (s p) then "s" else "x"

instance Show MapID where
    show m = (printf "%02x" (majorID m)) ++ ":" ++ (printf "%02x" (minorID m)) ++ " " ++ show (inodeID m)
instance Show Region where
    show m = out
      where
        fs =
            (printf "%08x" (rStartAddr m))
                ++ "-"
                ++ (printf "%08x" (rEndAddr m))
                ++ " "
                ++ show (rPermission m)
                ++ " "
                ++ (printf "%08x" (rOffset m))
                ++ " "
                ++ show (rMapID m)
        fsl =
            length fs
        ss = (replicate (73 - fsl) ' ') ++ (rFP m)
        out = fs ++ ss
instance Show MapInfo where
    show mi =
        "PID: "
            ++ show (miPID mi)
            ++ "\nExecutable name: "
            ++ miExecutableName mi
            ++ "\nRegions: \n"
            ++ showRegions (miRegions mi)

mapsFile :: PID -> String
mapsFile pid = printf "/proc/%d/maps" pid

exeFile :: PID -> String
exeFile pid = printf "/proc/%d/exe" pid

parseHex :: String -> Int
parseHex str = case readHex str of
    [(val, "")] -> val
    _ -> error "Invalid hex string"

-- The addresses in /proc/pid/maps is written as
-- start-end
-- where start and end are locations in memory written in HEX
getAddr :: String -> (Address, Address)
getAddr str
    | wlen < 2 = error ("Could not parse the following string into two addresses: " ++ str)
    | otherwise = (fromIntegral start, fromIntegral end)
  where
    wl = SPL.splitOn "-" str
    wlen = length wl
    start = parseHex (wl !! 0)
    end = parseHex (wl !! 1)

-- Permissions in /proc/pid/maps is written as
-- rwxp or ---s
-- r/- represent read/no read rPermission
-- w/- represent write/no write rPermission
-- x/- represent execute/no execute rPermission
-- p/s represent private or shared memory
getPermission :: String -> Permission
getPermission str = perm
  where
    rp = (str !! 0) == 'r'
    wp = (str !! 1) == 'w'
    xp = (str !! 2) == 'x'
    sp = (str !! 3) == 'p'
    perm = Permission rp wp xp sp

-- Offset is simply written as a hex number
getOffset :: String -> Address
getOffset str = fromIntegral $ parseHex str

-- ID is written as
-- major_id:minor_id inode_id
getID :: String -> String -> MapID
getID str1 str2
    | wlen < 2 = error ("Could not parse the following string into two IDS: " ++ str1)
    | otherwise = MapID majo mino ino
  where
    wl = SPL.splitOn ":" str1
    wlen = length wl
    majo = parseHex (wl !! 0)
    mino = parseHex (wl !! 1)
    ino = read str2

getFilePath :: [String] -> FilePath
getFilePath strs = unwords strs

-- Each line in /proc/pid/maps correspond to a region in virtual memory
-- This function takes a single line, and extracts the information as a Region type
processLine :: PID -> String -> Int -> Region
processLine pid line rid
    | wlen < 5 = error ("Could not parse maps file. The following line does not have 5 columns: \n" ++ line)
    | otherwise = mregion
  where
    seg = words line
    wlen = length seg
    (start_addr, end_addr) = getAddr (seg !! 0)
    perm = getPermission (seg !! 1)
    ofs = getOffset (seg !! 2)
    ids = getID (seg !! 3) (seg !! 4)
    filepath = getFilePath (drop 5 seg)
    mregion = Region{rStartAddr = start_addr, rEndAddr = end_addr, rPermission = perm, rOffset = ofs, rMapID = ids, rFP = filepath, rID = rid, rPID = pid}

processLines' :: PID -> [String] -> Int -> [Region]
processLines' _ [] _ = []
processLines' pid (str : rest) rid = this ++ that
  where
    this = [processLine pid str rid]
    that = processLines' pid rest (rid + 1)

processLines :: PID -> [String] -> [Region]
processLines pid l = processLines' pid l 0

parseFile :: PID -> FilePath -> IO [Region]
parseFile pid filepath = do
    content <- IO.readFile filepath
    let l = lines content
    let vas = processLines pid l
    return vas

-- Extracts Virtual memory information from /proc/pid/maps
getRegions :: PID -> IO [Region]
getRegions pid = parseFile pid (mapsFile pid)

getExecName :: PID -> IO String
getExecName pid = do
    execname <- Posix.readlink (exeFile pid)
    return execname

extractRegions :: PID -> IO MapInfo
extractRegions pid = do
    reg <- getRegions pid
    execname <- getExecName pid
    let info = MapInfo reg pid execname
    return info

totalBytes' :: [Region] -> Size
totalBytes' [] = 0
totalBytes' (y : ys) = this + that
  where
    this = fromIntegral ((rEndAddr y) - (rStartAddr y))
    that = totalBytes' ys

totalBytes :: MapInfo -> Size
totalBytes mapinfo = totalBytes' (miRegions mapinfo)

-- Filters for [Region]

filterRegions :: (Region -> Bool) -> MapInfo -> MapInfo
filterRegions f maps = maps{miRegions = rFiltered_miRegions}
  where
    rFiltered_miRegions = filter f (miRegions maps)

rFilterRW :: Region -> Bool
rFilterRW region = readP && writeP
  where
    readP = (r (rPermission region))
    writeP = (w (rPermission region))

rFilterR :: Region -> Bool
rFilterR region = readP
  where
    readP = (r (rPermission region))

rFilterW :: Region -> Bool
rFilterW region = writeP
  where
    writeP = (w (rPermission region))

rFilterMappings :: String -> Region -> Bool
rFilterMappings execname region = not_mapping || not_exec
  where
    not_mapping = ((inodeID . rMapID) $ region) == 0
    not_exec = (rFP region == execname)

defaultRFilter :: MapInfo -> (Region -> Bool)
defaultRFilter mapinfo = func
  where
    func reg = rwf && maf
      where
        rwf = rFilterRW reg
        maf = rFilterMappings (miExecutableName mapinfo) reg
