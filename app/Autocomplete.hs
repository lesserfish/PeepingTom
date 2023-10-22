module Autocomplete (
    ErrorMsg (..),
    getAction,
    autocomplete,
) where

import Commands
import Data.List (isPrefixOf)
import Debug.Trace
import State
import System.Console.Haskeline
import Text.Printf

completeHelper :: String -> [String] -> [Completion]
completeHelper str wordList = map simpleCompletion $ filter (str `isPrefixOf`) wordList

getCommandByStr :: [Command] -> String -> Maybe Command
getCommandByStr commands name
    | length matches == 0 = Nothing
    | otherwise = Just (matches !! 0)
  where
    matches = filter (\c -> (cName c) == name) commands

recursiveSuggestion :: [String] -> [Command] -> [Command]
recursiveSuggestion [] cmd = cmd
recursiveSuggestion str [] = []
recursiveSuggestion (str : rest) commands = output
  where
    maybecmd = getCommandByStr commands str
    output = case maybecmd of
        Nothing -> []
        Just (ACommand name act hlp) -> if length rest > 0 then recursiveSuggestion rest [(ACommand name act hlp)] else []
        Just (HCommand name sub _) -> recursiveSuggestion rest sub

autocomplete :: [Command] -> (String -> String -> IO [Completion])
autocomplete commandList prevstr str = do
    let prev = words . reverse $ prevstr
    let candidates = recursiveSuggestion prev commandList
    let cnames = map cName candidates
    return $ completeHelper str cnames

data ErrorMsg = ErrorMsg String

-- Todo: Improve error handling
getAction :: [String] -> [Command] -> Either ErrorMsg ([String], ([String] -> State -> IO State))
getAction [] [(ACommand _ act _)] = Right ([], act)
getAction [name] cmds = output
  where
    maybecmd = getCommandByStr cmds name
    output = case maybecmd of
        Nothing -> Left (ErrorMsg (printf "Could not understand '%s'" name))
        Just (ACommand _ act _) -> Right ([], act)
        Just (HCommand _ sc hlp) -> Left (ErrorMsg (printf "%s expected additional arguments.\n%s" name hlp))
getAction (name : rest) cmds = output
  where
    maybecmd = getCommandByStr cmds name
    output = case maybecmd of
        Nothing -> Left (ErrorMsg (printf "Could not understand '%s'" name))
        Just (ACommand _ act _) -> Right (rest, act)
        Just (HCommand _ sc _) -> getAction rest sc
getAction _ _ = Left (ErrorMsg "Could not understand input.")
