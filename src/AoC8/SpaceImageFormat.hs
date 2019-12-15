module AoC8.SpaceImageFormat where

import Data.List.Split
import Data.List
import System.IO
import Data.Char

main :: IO ()
main = do
  handle <- openFile "src/AoC8/input.txt" ReadMode
  contents <- hGetContents handle
  print $ onesTimesTwos $ spaceImageFormat $ parse contents
  --print $ findBestPhaseSequence $ parseToList contents

parse :: String -> [Int]
parse = map digitToInt

onesTimesTwos :: [Int] -> Int
onesTimesTwos xs = let ones = numN 1 xs; twos = numN 2 xs in
    ones * twos

spaceImageFormat :: [Int] -> [Int]
spaceImageFormat = minimumBy (\x y -> compare (numZeros x) (numZeros y)) .  chunksOf (25*6)

numZeros :: [Int] -> Int
numZeros = length . filter (==0)

numN :: Int -> [Int] -> Int
numN n = length . filter (==n)