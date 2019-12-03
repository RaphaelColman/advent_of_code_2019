module AoC2.ProgramAlarm where

intcode :: [Int] -> [Int]
intcode xs = go xs xs 
  where go [] newList = newList
        go (a:b:c:d:xs) newList' = go (drop 4 mutateList) mutateList
          where mutateList = (applyOperation a b c d newList')
        go (y:ys) newList'' = newList''

applyOperation :: Int -> Int -> Int -> Int -> [Int] -> [Int]
applyOperation o f s l xs
  | o == 1 = replaceInList l ((xs !! f) + (xs !! s)) xs
  | o == 2 = replaceInList l ((xs !! f) * (xs !! s)) xs
  | o == 99 = xs

replaceInList :: Int -> a -> [a] -> [a]
replaceInList index value xs = take index xs ++ [value] ++ drop (index + 1) xs

sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)
