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
    Map,
    getMap,
    rwFilter,
    notMapping,
    debug, -- TODO: Remove this line
) where

import qualified Data.List.Split as SPL
import Data.Word
import Numeric (readHex)
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
type Map = [Region]

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
            "\n"
                ++ (printf "%08x" (startAddress m))
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

processLines' :: Int -> [String] -> Int -> Map
processLines' _ [] _ = []
processLines' pid (str : rest) rid = this ++ that
  where
    this = [processLine pid str rid]
    that = processLines' pid rest (rid + 1)

processLines :: Int -> [String] -> Map
processLines pid l = processLines' pid l 0

parseFile :: Int -> FilePath -> IO Map
parseFile pid filepath = do
    content <- IO.readFile filepath
    let l = lines content
    let vas = processLines pid l
    return vas

-- Extracts Virtual memory information from /proc/pid/maps
getMap :: Int -> IO Map
getMap _pid = parseFile _pid ("/proc/" ++ show _pid ++ "/maps")

-- Filter to select only the memory regions with read/write permissions.
-- These are the regions of interest where we will search for our variables
rwFilter :: Region -> Bool
rwFilter mregion = readP && writeP
  where
    readP = case (r (permission mregion)) of
        R -> True
        NR -> False
    writeP = case (w (permission mregion)) of
        W -> True
        NW -> False

notMapping :: Region -> Bool
notMapping mregion = (offset mregion) == 0

debug :: Int -> Int -> IO ()
debug _pid i = do
    content <- IO.readFile ("/proc/" ++ show _pid ++ "/maps")
    let l = lines content
    let vas = processLines _pid l
    putStrLn $ l !! i
    putStrLn $ show (vas !! i)
    return ()
