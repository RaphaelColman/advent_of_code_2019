module AoC3.CrossedWires where

closestIntersection :: String -> Integer
closestIntersection = undefined

allPointsFromLine :: [String] -> [(Integer, Integer)]
allPointsFromLine = undefined

up :: (Num a, Enum a, Num b, Enum b) => b -> (a, b) -> [(a, b)]
up amount (startx,starty) = [(x,y) | x <- [startx], y <- [starty+1..starty + amount]]

right :: (Num a, Enum a, Num b, Enum b) => a -> (a, b) -> [(a, b)]
right amount (startx,starty) = [(x,y) | x <- [startx+1..startx + amount], y <- [starty]]

