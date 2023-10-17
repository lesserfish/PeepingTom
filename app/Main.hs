module Main where

import PeepingTom.State
import System.IO

main :: IO ()
main = do
    PeepingTom.State.debug
    return ()
