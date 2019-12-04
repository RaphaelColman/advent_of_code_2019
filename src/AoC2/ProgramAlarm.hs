module AoC2.ProgramAlarm where

import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ intcode (splitToIntList "," contents)

splitToIntList :: String -> String -> [Int]
splitToIntList delim strList = map read $ splitOn delim strList

intcode :: [Int] -> [Int]
intcode xs = go xs 0 
  where go [] _ = []
        go xs i
            | i >= (length xs) = xs
            | xs !! i == 99 = xs
            | otherwise = go (intcodeAtIndex xs i) (i+4)

applyOperation :: Int -> Int -> Int -> Int -> [Int] -> [Int]
applyOperation o f s l xs
  | o == 1 = replaceInList l ((xs !! f) + (xs !! s)) xs
  | o == 2 = replaceInList l ((xs !! f) * (xs !! s)) xs
  | o == 99 = error ("Should not have got opcode 99 in here: " ++ show o)
  | otherwise = error ("Unknown opcode: " ++ show o)

intcodeAtIndex :: [Int] -> Int -> [Int]
intcodeAtIndex xs i = let slice = sliceList i (i+4) xs
                      in go slice xs
                     where go (a:b:c:d:as) xs = applyOperation a b c d xs
                           go h i = error (show h ++ show i)

replaceInList :: Int -> a -> [a] -> [a]
replaceInList index value xs = take index xs ++ [value] ++ drop (index + 1) xs

sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)
