module AoC2.ProgramAlarm where

intcode :: (Num a) => [a] -> [a]
intcode xs = [1,2,3,4]

applyIntcode :: (Num a) => [a] -> Integer -> [a]
applyIntcode = undefined

data Operation = Operation {operation :: Integer, first :: Integer, second :: Integer, location :: Integer} deriving (Show)
