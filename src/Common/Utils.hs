module Common.Utils where

import           Control.Lens
import           Data.Array
import           Data.List
import           Data.List.Split
import           Data.Map        (Map (..))
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Sequence   (Seq (..))
import qualified Data.Sequence   as Seq
import           Data.Set        (Set (..))
import qualified Data.Set        as Set
import           Linear.V2
import           Linear.V3

seqLast :: Seq a -> Maybe a
seqLast s = let i = Seq.length s - 1 in
      Seq.lookup i s


-- If the index is too large, then extend the sequence with 0
seqUpdateAndExtend :: Int -> Int -> Seq Int -> Seq Int
seqUpdateAndExtend i a s = if i < ln then Seq.update i a s else Seq.update i a extendedSeq
      where extendedSeq = s Seq.>< Seq.replicate (i - ln + 1) 0
            ln = Seq.length s

seqLookupWithDefault :: a -> Int -> Seq a -> a
seqLookupWithDefault defaultValue i s = if i < ln then fromJust (Seq.lookup i s) else defaultValue
      where ln = Seq.length s


gcf :: Integer -> Integer -> Integer
gcf a b
      | a == 0 || b == 0 = 1
      | otherwise = let larger = abs $ max a b;
                        smaller = abs $ min a b;
                        (_, remainder) = quotRem larger smaller in
      if remainder == 0
      then b
      else gcf smaller remainder

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0,1..]

enumerateMultilineString :: String -> [((Int, Int), Char)]
enumerateMultilineString str
      | maximum lengths /= minimum lengths = error "Line lengths are not equal"
      | otherwise = zip coords (concat lines')
      where lines' = lines str
            xLength = length (head lines')
            yLength = length lines'
            lengths = map length lines'
            coords = [(x,y) | y <- [0..yLength -1], x <- [0..xLength - 1]]

enumNext :: (Enum a, Eq a, Bounded a) => a -> a
enumNext e
      | e == maxBound  = minBound
      | otherwise = succ e


enumPrev :: (Enum a, Eq a, Bounded a) => a -> a
enumPrev e
      | e == minBound = maxBound
      | otherwise = pred e

v2ToTup :: V2 Int -> (Int, Int)
v2ToTup (V2 x y) = (x, y)

foldrV3 :: (a -> b -> b) -> b -> V3 a -> b
foldrV3 f acc (V3 x y z) = foldr f acc [x,y,z]

clearCharacters :: (Char -> Bool) -> String -> String
clearCharacters p = map (\c -> if p c then ' ' else c)

firstRepeatedIndex :: Ord a => [a] -> Maybe Int
firstRepeatedIndex xs = go (enumerate xs) Set.empty
      where go :: Ord a => [(Int, a)] -> Set a -> Maybe Int
            go [] _ = Nothing
            go (x:xs') found = if snd x `Set.member` found
                              then Just (fst x)
                              else go xs' (Set.insert (snd x) found)

renderVectorMap :: Map (V2 Int) Char -> String
renderVectorMap m = foo
    where keys = Map.keys m
          xMax = maximumBy (\a b -> compare (a ^._x) (b ^._x)) keys ^._x
          xMin = minimumBy (\a b -> compare (a ^._x) (b ^._x)) keys ^._x
          yMax = maximumBy (\a b -> compare (a ^._y) (b ^._y)) keys ^._y
          yMin = minimumBy (\a b -> compare (a ^._y) (b ^._y)) keys ^._y
          xRange = (xMax - xMin) + 1
          panelList = [Map.findWithDefault '.' (V2 x y) m | y <- [yMin .. yMax], x <- [xMin..xMax]]
          panelRows = chunksOf xRange panelList
          foo = unlines (replicate xRange '=' : panelRows)

safeHead :: [a] -> Maybe a
safeHead xs
      | null xs = Nothing
      | otherwise  = Just $ head xs

firstPairIndex :: Eq a => [a] -> Maybe (Int, Int)
firstPairIndex = go 0
    where go :: Eq a => Int -> [a] -> Maybe (Int, Int)
          go i xs'
            | null newList = Nothing
            | length found < 2 = go (i+1) xs'
            | otherwise = Just (head found, found !! 1)
                where newList = drop i xs'
                      found = elemIndices (head newList) xs'


dropBetween :: Int -> Int -> [a] -> [a]
dropBetween start end xs = take start xs ++ drop end xs