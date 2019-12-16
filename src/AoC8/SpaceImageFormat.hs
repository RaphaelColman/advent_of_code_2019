module AoC8.SpaceImageFormat where

import Data.List.Split
import Data.List
import System.IO
import Data.Char
import qualified Data.Array as Array
import Data.Array(Array(..))
import Control.Applicative

type Layer = Array (Int, Int) Int

main :: IO ()
main = do
  handle <- openFile "src/AoC8/input.txt" ReadMode
  contents <- hGetContents handle
  print $ onesTimesTwos $ spaceImageFormat $ parse contents
  putStr $ layerToString $ resolveLayers $ makeLayers $ parse contents

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

makeLayer :: [Int] -> Layer
makeLayer = Array.listArray ((0,0), (5,24)) 

makeLayers :: [Int] -> [Layer] 
makeLayers = map makeLayer . chunksOf (25*6)

resolveTwoLayers :: Layer -> Layer -> Layer
resolveTwoLayers = zipWithArr (\x y -> if x == 2 then y else x)

zipWithArr :: Array.Ix a1 => (a2 -> b -> e) -> Array.Array a1 a2 -> Array.Array a1 b -> Array.Array a1 e
zipWithArr f xs ys = Array.listArray (Array.bounds xs) $ fmap (liftA2 f (xs Array.!) (ys Array.!)) (Array.range (Array.bounds xs))

resolveLayers :: [Layer] -> Layer
resolveLayers = foldl1 resolveTwoLayers

layerToString :: Layer -> String
layerToString l =  concatMap printCoordinate (Array.range (Array.bounds l))
  where printCoordinate (y,x) = let value = l Array.! (y,x);
                                    newline = if x==24 then "\n" else "" 
                                    stringValue = if value == 0 then "-" else "#" in
                                      stringValue ++ newline
