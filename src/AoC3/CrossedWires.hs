module AoC3.CrossedWires where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import System.IO

type Point = (Integer, Integer)
type Line = [Point]

main :: IO()
main = do
  handle <- openFile "src/AoC3/input.txt" ReadMode  
  contents <- hGetContents handle
  --putStrLn $ show $ closestIntersection contents
  putStrLn $ show $ shortestIntersection contents

parseInput :: String -> [Line]
parseInput = (map line) . (map (\x -> splitOn "," x)) . (lines)

closestIntersection :: String -> Maybe Integer
closestIntersection = safeMinimum 
                    . map (manhattan (0,0))
                    . allCrossings
                    . parseInput

shortestIntersection :: String -> Maybe Int
shortestIntersection input = let l1 = (parseInput input) !! 0; l2 = (parseInput input) !! 1 in
                                 shortestTime l1 l2 (crossings l1 l2)

allCrossings :: [Line] -> [Point]
allCrossings = concat
              . (map (uncurry crossings))
              . pairs

crossings :: Line -> Line -> [Point]
crossings l1 l2 = (filter (\x -> x /= (0,0)))
                $ (S.toList $ S.intersection (S.fromList l1) (S.fromList l2))

line :: [String] -> Line 
line = foldl doLine [(0,0)]
  where doLine = (\x y -> x ++ ((parseDirection (head y)) (read (tail y) :: Integer) (last x)))

up :: (Num a, Enum a, Num b, Enum b) => b -> (a, b) -> [(a, b)]
up amount (startx,starty) = [(x,y) | x <- [startx], y <- [starty+1..starty + amount]]

down :: (Num a, Enum a, Num b, Enum b) => b -> (a, b) -> [(a, b)]
down amount (startx, starty) = [(x,y) | x <- [startx], y <- [starty-1, starty-2..starty-amount]]

right :: (Num a, Enum a, Num b, Enum b) => a -> (a, b) -> [(a, b)]
right amount (startx,starty) = [(x,y) | x <- [startx+1..startx + amount], y <- [starty]]

left :: (Num a, Enum a, Num b, Enum b) => a -> (a, b) -> [(a, b)]
left amount (startx, starty) = [(x,y) | x <- [startx-1,(startx-2)..startx-amount], y <- [starty]]

parseDirection :: (Num a, Enum a) => Char -> (a -> (a, a) -> [(a, a)])
parseDirection dir = case dir of
                       'R' -> right
                       'L' -> left
                       'U' -> up
                       'D' -> down

manhattan :: (Integer, Integer) -> (Integer, Integer) -> Integer
manhattan (a, b) (c, d) = abs((c-a)) + abs((d-b))

freqs :: (Ord k, Num a) => [k] -> M.Map k a
freqs xs = M.fromListWith (+) $ (map (\x -> (x, 1)) xs)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just (minimum xs)

stepsForLine :: Point -> Line -> Maybe Int
stepsForLine p l = elemIndex p l

-- Holy shit I get to use applicatives
time :: Line -> Line -> Point -> Maybe Int
time l1 l2 p = (+) <$> (stepsForLine p l1) <*> (stepsForLine p l2)

shortestTime :: Line -> Line -> [Point] -> Maybe Int
shortestTime l1 l2 xs = ((sequence . (map (time l1 l2))) xs) >>= safeMinimum

