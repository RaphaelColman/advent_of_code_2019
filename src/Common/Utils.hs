module Common.Utils where

import qualified Data.Sequence as Seq
import Data.Sequence(Seq(..))
import Data.Maybe

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