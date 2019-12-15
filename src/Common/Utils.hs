module Common.Utils where

import qualified Data.Sequence as Seq
import Data.Sequence(Seq(..))

--Example of recursively tying the knot
seqLast :: Seq a -> Maybe a
seqLast s = let i = Seq.length s - 1 in
      Seq.lookup i s