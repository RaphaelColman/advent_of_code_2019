module AoC9.SensorBoost where

import Common.Intcode
import System.IO

main :: IO ()
main = do
    handle <- openFile "src/AoC9/input.txt" ReadMode
    contents <- hGetContents handle
    print $ runIntCodeWithInput [1] $ parse contents
    print $ runIntCodeWithInput [2] $ parse contents