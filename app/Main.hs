module Main where

import qualified PeepingTom.Experimental.PT as PT
import qualified PeepingTom.Filters as Filters
import PeepingTom.Internal
import qualified PeepingTom.Maps as Maps
import qualified PeepingTom.Posix as Posix
import PeepingTom.State
import PeepingTom.Type
import qualified PeepingTom.Writer as Writer
import System.IO
import Text.Printf (printf)

m = 1
pid = 374069
main1 :: IO ()
main1 = do
    putStrLn $ printf "PID: %d" pid
    all_maps <- Maps.getMapInfo pid
    let maps = Maps.filterMap (Maps.defaultFilter all_maps) all_maps
    let fltr_41 = Filters.eqInt 41
    peepstate <- scanMap [Int32] fltr_41 maps
    putStrLn $ "[DEBUG] This is the initial extraction of Int64 Types equal to 41"
    putStrLn $ "[DEBUG] Please compare this result with scanmem!"
    putStrLn $ showState (-1) 0 peepstate

{--
    putStrLn $ "[DEBUG] After the results have been checked, please alter the candidates 0..200 to value 43! Please enter once complete...."
    _ <- getChar
    putStrLn $ "[DEBUG] We are now showing the candidates which still have value equal 41. Please compare the results with scanmem!"
    peepstate2 <- updateState 4096 peepstate
    let peepstate41 = applyFilter fltr_41 peepstate2
    putStrLn $ showState 5 0 peepstate41
    putStrLn $ "[DEBUG] We are now showing the candidates which have a value equal to 43. Please compare the results with scanmem! Please enter once complete!"
    let peepstate43 = applyFilter (Filters.eqInt 43) peepstate2
    putStrLn $ showState 5 0 peepstate43
    _ <- getChar
    putStrLn $ "[DEBUG] We are now updating the candidates with value equal to 43. We are setting the new value equal to 45! Press enter to continue:"
    _ <- getChar
    peepstate45 <- applyWriter (Writer.writeInt 45) peepstate43
    putStrLn $ "[DEBUG] This is the result after updating the values! Please compare the results with scanmem!"
    putStrLn $ showState 5 0 peepstate45
    _ <- getChar
    putStrLn $ "[DEBUG] This is the end of the Debug! Please set all of the remaining candidates in scanmem equal to 41. Hope it worked out!"
    _ <- getChar
    return ()
--}
main2 :: IO ()
main2 = PT.debug2 pid

main = if m == 1 then main1 else main2
