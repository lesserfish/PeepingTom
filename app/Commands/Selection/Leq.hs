module Commands.Selection.Leq where

import Commands.Base
import Commands.Selection.Helper
import qualified PeepingTom.Filters as PTFilter
import State

leqAction :: [String] -> State -> IO State
leqAction args state = do
    if length args /= 1
        then do
            putStrLn $ "Error: <= requires one and only one additional argument: The integer value to be used in the comparison!"
            putStrLn $ cmdHelp
            return state
        else do
            let numstr = args !! 0
            let num = read numstr :: Integer
            let fltr = PTFilter.compareInt (<= num)
            new_state <- scanAction fltr state
            return new_state

leqHelp :: String
leqHelp = cmdHelp

leqCommand :: Command
leqCommand =
    ACommand
        { cName = "<="
        , action = leqAction
        , help = leqHelp
        }
