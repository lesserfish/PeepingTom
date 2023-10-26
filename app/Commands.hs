module Commands (
    Command (..),
    commands,
) where

import Commands.Base
import Commands.Help
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

import Control.Exception
import State
import Text.Read

commands = [cmdCommand, newCommand, pidCommand, helpCommand, listCommand, updateCommand, intSetCommand, loadCommand, saveCommand, setCommand, resetCommand]
