module AoC2.ProgramAlarm where

intcode :: (Num a) => [a] -> [a]
intcode xs = [1,2,3,4]

applyIntcode :: (Num a) => [a] -> Integer -> [a]
applyIntcode = undefined
applyIntcode xs index = sliceList index (index + 4) xs

applyOperation :: Operation -> [Int] -> [Int]
applyOperation (Operation o f s l) xs
  | o == 1 = replaceInList l (f+s)
  | o == 2 = replaceInList l (f*s)
  | o == 99 = xs
  where replaceInList index value = take index xs ++ [value] ++ drop (index + 1) xs

sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

data Operation = Operation {operation :: Int, first :: Int, second :: Int, location :: Int} deriving (Show)
