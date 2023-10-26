module Commands.Help (
    helpCommand,
) where

import Commands.Base
import Commands.IntSet
import Commands.List
import Commands.Load
import Commands.New
import Commands.PID
import Commands.Reset
import Commands.Save
import Commands.Selection
import Commands.Set
import Commands.Update
import State
import Text.Printf

commands = [cmdCommand, newCommand, pidCommand, helpCommand, listCommand, updateCommand, intSetCommand, loadCommand, saveCommand, setCommand, resetCommand]

getCommandByStr :: [Command] -> String -> Maybe Command
getCommandByStr commands name
    | length matches == 0 = Nothing
    | otherwise = Just (matches !! 0)
  where
    matches = filter (\c -> (cName c) == name) commands

getHelp :: [String] -> [Command] -> String
getHelp [] [(ACommand _ _ hlp)] = hlp
getHelp [name] cmds = output
  where
    maybecmd = getCommandByStr cmds name
    output = case maybecmd of
        Nothing -> printf "Could not understand '%s'. Type 'help command' to get help" name
        Just (ACommand _ _ hlp) -> hlp
        Just (HCommand _ sc hlp) -> hlp
getHelp (name : rest) cmds = output
  where
    maybecmd = getCommandByStr cmds name
    output = case maybecmd of
        Nothing -> (printf "Could not understand '%s'. Type 'help' to get help" name)
        Just (ACommand _ _ hlp) -> hlp
        Just (HCommand _ sc _) -> getHelp rest sc
getHelp _ _ = "Could not understand input. Type 'help' to get help."
helpHelp :: String
helpHelp =
    "\nPeepingTom: A Virtual memory scanner.\n\nThe following commands are available: \
    \\n\t pid:                         Sets the PID of the process to be scanned by PeepingTom. \
    \\n\t $ [filter] [value]:          Scans the memory or update the candidates, and extracts those that satisfy the filter. \
    \\n\t set [option] [args]:         Sets various options regarding the scan. Type 'help set' to see more.\
    \\n\t list [object]:               Lists several objects. To see a list of available objects type 'help list'.\
    \\n\t update:                      Update the values of the current candidates.\
    \\n\t save [name]:                 Save the current list of candidates.\
    \\n\t load [name]:                 Loads a list of candidates.\
    \\n\t new [name]:                  Creates a new list of candidates.\
    \\n\t reset:                       Resets the current list of candidates.\n\n"

helpAction :: [String] -> State -> IO State
helpAction args state = do
    if length args == 0
        then do
            putStrLn $ helpHelp
            return state
        else do
            putStrLn $ getHelp args commands
            return state

helpCommand :: Command
helpCommand =
    ACommand
        { cName = "help"
        , action = helpAction
        , help = helpHelp
        }
