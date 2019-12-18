module Common.Utils where

import qualified Data.Sequence as Seq
import Data.Sequence(Seq(..))
import Data.Maybe
import Data.Array
import Debug.Trace

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
gcf a b = let larger = abs $ max a b;
              smaller = abs $ min a b;
              (_, remainder) = quotRem larger smaller in
      if remainder == 0
      then b
      else gcf smaller remainder

enumerate :: [a] -> [(Int, a)]
enumerate xs = map (\x -> (x, xs !! x)) [0..(ln-1)] 
    where ln = length xs

enumerateMultilineString :: String -> [((Int, Int), Char)] 
enumerateMultilineString str 
      | maximum lengths /= minimum lengths = error "Line lengths are not equal" 
      | otherwise = zip (range ((0,0), (xLength -1, yLength -1))) (concat lines') 
      where lines' = lines str 
            xLength = length (head lines')
            yLength = length lines'
            lengths = map length lines'