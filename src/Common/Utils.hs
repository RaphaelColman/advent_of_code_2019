module Common.Utils where

import Data.Array as Array
import qualified Data.Sequence as Seq
import Data.Sequence(Seq(..))

--Example of recursively tying the knot
basic :: (Eq a) => [a] -> [a] -> Int
basic a b = d m n
    where (m, n) = (length a, length b)
          d i 0 = i
          d 0 j = j
          d i j
            | a !! (i - 1) ==  b !! (j - 1) = ds ! (i - 1, j - 1)
            | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]
          ds = Array.listArray bounds
                [d i j | (i, j) <- Array.range bounds]
          bounds = ((0, 0), (m, n))

seqLast :: Seq a -> Maybe a
seqLast s = let i = Seq.length s - 1 in
      Seq.lookup i s