module Commands.Selection.Lt where

import Commands.Base
import Commands.Selection.Helper
import qualified PeepingTom.Filters as PTFilter
import State

ltAction :: [String] -> State -> IO State
ltAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: < requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let fltr = PTFilter.compareInt (< num)
            new_state <- scanAction fltr state
            return new_state

ltHelp :: String
ltHelp = cmdHelp

ltCommand :: Command
ltCommand =
    ACommand
        { cName = "<"
        , action = ltAction
        , help = ltHelp
        }
