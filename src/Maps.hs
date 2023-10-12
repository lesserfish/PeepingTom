module Maps (
    R (..),
    W (..),
    X (..),
    S (..),
    Permission (..),
    Address,
    Offset,
    MapID (..),
    Region (..),
    Regions,
    MapInfo (..),
    getRegions,
    getMapInfo,
    filterMap,
    filterRW,
    filterMappings,
    defaultFilter,
) where

import qualified Data.List.Split as SPL
import Numeric (readHex)
import qualified Posix as P
import qualified System.IO as IO
import Text.Printf (printf)

data R = R | NR deriving (Show)
data W = W | NW deriving (Show)
data X = X | NX deriving (Show)
data S = S | P deriving (Show)
data Permission = Permission {r :: R, w :: W, x :: X, s :: S}
type Address = Int
type Offset = Int
data MapID = MapID {majorID :: Int, minorID :: Int, inodeID :: Int}
data Region = Region {startAddress :: Address, endAddress :: Address, permission :: Permission, offset :: Offset, mapID :: MapID, fp :: FilePath, regionID :: Int, rPID :: Int}
type Regions = [Region]
data MapInfo = MapInfo {regions :: Regions, mPID :: Int, executableName :: String}

showRegions :: Regions -> String
showRegions [] = ""
showRegions (y : ys) = show y ++ "\n" ++ (showRegions ys)

instance Show Permission where
    show p = sr ++ sw ++ sx ++ ss
      where
        sr = case (r p) of
            R -> "r"
            NR -> "-"
        sw = case (w p) of
            W -> "w"
            NW -> "-"
        sx = case (x p) of
            X -> "x"
            NX -> "-"
        ss = case (s p) of
            S -> "s"
            P -> "p"
instance Show MapID where
    show m = (printf "%02x" (majorID m)) ++ ":" ++ (printf "%02x" (minorID m)) ++ " " ++ show (inodeID m)
instance Show Region where
    show m = out
      where
        fs =
            (printf "%08x" (startAddress m))
                ++ "-"
                ++ (printf "%08x" (endAddress m))
                ++ " "
                ++ show (permission m)
                ++ " "
                ++ (printf "%08x" (offset m))
                ++ " "
                ++ show (mapID m)
        fsl =
            length fs
        ss = (replicate (73 - fsl) ' ') ++ (fp m)
        out = fs ++ ss
instance Show MapInfo where
    show mi =
        "PID: "
            ++ show (mPID mi)
            ++ "\nExecutable name: "
            ++ executableName mi
            ++ "\nRegions: \n"
            ++ showRegions (regions mi)

mapsFile :: Int -> String
mapsFile pid = printf "/proc/%d/maps" pid

exeFile :: Int -> String
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
    | otherwise = (start, end)
  where
    wl = SPL.splitOn "-" str
    wlen = length wl
    start = parseHex (wl !! 0)
    end = parseHex (wl !! 1)

-- Permissions in /proc/pid/maps is written as
-- rwxp or ---s
-- r/- represent read/no read permission
-- w/- represent write/no write permission
-- x/- represent execute/no execute permission
-- p/s represent private or shared memory
getPermission :: String -> Permission
getPermission str = perm
  where
    rp = if (str !! 0) == 'r' then R else NR
    wp = if (str !! 1) == 'w' then W else NW
    xp = if (str !! 2) == 'x' then X else NX
    sp = if (str !! 3) == 'p' then P else S
    perm = Permission rp wp xp sp

-- Offset is simply written as a hex number
getOffset :: String -> Offset
getOffset str = parseHex str

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
processLine :: Int -> String -> Int -> Region
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
    mregion = Region{startAddress = start_addr, endAddress = end_addr, permission = perm, offset = ofs, mapID = ids, fp = filepath, regionID = rid, rPID = pid}

processLines' :: Int -> [String] -> Int -> Regions
processLines' _ [] _ = []
processLines' pid (str : rest) rid = this ++ that
  where
    this = [processLine pid str rid]
    that = processLines' pid rest (rid + 1)

processLines :: Int -> [String] -> Regions
processLines pid l = processLines' pid l 0

parseFile :: Int -> FilePath -> IO Regions
parseFile pid filepath = do
    content <- IO.readFile filepath
    let l = lines content
    let vas = processLines pid l
    return vas

-- Extracts Virtual memory information from /proc/pid/maps
getRegions :: Int -> IO Regions
getRegions pid = parseFile pid (mapsFile pid)

getExecName :: Int -> IO String
getExecName pid = do
    execname <- P.readlink (exeFile pid)
    return execname

getMapInfo :: Int -> IO MapInfo
getMapInfo pid = do
    reg <- getRegions pid
    execname <- getExecName pid
    let info = MapInfo reg pid execname
    return info

-- Filters

filterMap :: (Region -> Bool) -> MapInfo -> MapInfo
filterMap f maps = maps{regions = filtered_regions}
  where
    filtered_regions = filter f (regions maps)

filterRW :: Region -> Bool
filterRW region = readP && writeP
  where
    readP = case (r (permission region)) of
        R -> True
        NR -> False
    writeP = case (w (permission region)) of
        W -> True
        NW -> False

filterMappings :: String -> Region -> Bool
filterMappings execname region = not_mapping || not_exec
  where
    not_mapping = ((inodeID . mapID) $ region) == 0
    not_exec = (fp region == execname)

-- not_exec = trace (printf "execname = '%s' : '%s' = filepath" (fp region) execname) (fp region == execname)

defaultFilter :: MapInfo -> (Region -> Bool)
defaultFilter mapinfo = func
  where
    func reg = rwf && maf
      where
        rwf = filterRW reg
        maf = filterMappings (executableName mapinfo) reg
