module AoC3.CrossedWires where

import qualified Data.Map as Map

closestIntersection :: String -> Integer
closestIntersection = undefined

allPointsFromLine :: [String] -> [(Integer, Integer)]
allPointsFromLine = undefined

up :: (Num a, Enum a, Num b, Enum b) => b -> (a, b) -> [(a, b)]
up amount (startx,starty) = [(x,y) | x <- [startx], y <- [starty+1..starty + amount]]

down :: (Num a, Enum a, Num b, Enum b) => b -> (a, b) -> [(a, b)]
down amount (startx, starty) = [(x,y) | x <- [startx], y <- [starty-1, starty-2..starty-amount]]

right :: (Num a, Enum a, Num b, Enum b) => a -> (a, b) -> [(a, b)]
right amount (startx,starty) = [(x,y) | x <- [startx+1..startx + amount], y <- [starty]]

left :: (Num a, Enum a, Num b, Enum b) => a -> (a, b) -> [(a, b)]
left amount (startx, starty) = [(x,y) | x <- [startx-1,(startx-2)..startx-amount], y <- [starty]]

parseDirection :: (Num a, Enum a) => [Char] -> (a -> (a, a) -> [(a, a)])
parseDirection dir = case dir of
                       "R" -> right
                       "L" -> left
                       "U" -> up
                       "D" -> down
