module Commands where

import CLI
import Text.Printf

-- List

listRegionBananaAction :: [String] -> State -> IO State
listRegionBananaAction args st = do
    putStrLn $ printf "Received a call for 'list region banana' with the following arguments: %s" (show args)
    return st

listRegionAppleAction :: [String] -> State -> IO State
listRegionAppleAction args st = do
    putStrLn $ printf "Received a call for 'list region apple' with the following arguments: %s" (show args)
    return st

listRegionBanaCommand :: Command
listRegionBanaCommand = ACommand "banana" listRegionBananaAction ""

listRegionAppleCommand :: Command
listRegionAppleCommand = ACommand "apple" listRegionAppleAction ""

listRegionCommand :: Command
listRegionCommand = HCommand "region" [listRegionBanaCommand, listRegionAppleCommand] "list region\n\nUsage:\n\tlist region [apple, banana]\n\n"

listMapAction :: [String] -> State -> IO State
listMapAction args st = do
    putStrLn $ printf "Received a call for 'list map' with the following arguments: %s" (show args)
    return st

listMapCommand :: Command
listMapCommand = ACommand "map" listMapAction "list map\n\nUsage:\n\tlist map\n\n"

listCommand :: Command
listCommand = HCommand "list" [listMapCommand, listRegionCommand] "list\n\nUsage:\n\tlist map\n\tlist region [apple, banana]\n\n"

cmdAction :: [String] -> State -> IO State
cmdAction args st = do
    putStrLn $ printf "Received a call for '$' with the following arguments: %s" (show args)
    return st

cmdCommand :: Command
cmdCommand = ACommand "$" cmdAction ""
