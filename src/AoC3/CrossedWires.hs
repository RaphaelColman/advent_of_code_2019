module AoC3.CrossedWires where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split

parseInput :: String -> [[String]]
parseInput = (map (\x -> splitOn "," x)) . (splitOn "\n")

closestIntersection :: String -> Integer
closestIntersection = closestIntersectionImpl . parseInput

closestIntersectionImpl :: [[String]] -> Integer
closestIntersectionImpl = minimum
                        . (map $ manhattan (0,0))
                        . (filter (/= (0,0)))
                        . allCrossedPoints

allCrossedPoints :: [[String]] -> [(Integer, Integer)]
allCrossedPoints xs = map (\x -> fst x) $ M.toList $ M.filter (>1) $  freqs $ allPointsFromLines xs

allPointsFromLines :: [[String]] -> [(Integer, Integer)]
allPointsFromLines xs = concat $ map line xs

-- I need a cartesian function. Applicatives don't work because they combine with themselves

crossings :: Line -> Line -> [Point]
crossings l1 l2 = (filter (\x -> x /= (0,0)))
                $ (S.toList $ S.intersection (S.fromList l1) (S.fromList l2))

type Point = (Integer, Integer)
type Line = [Point]

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
manhattan (a, b) (c, d) = abs $ (c-a) + (d-b)

freqs :: (Ord k, Num a) => [k] -> M.Map k a
freqs xs = M.fromListWith (+) $ (map (\x -> (x, 1)) xs)
