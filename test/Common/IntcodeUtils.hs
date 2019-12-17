module Common.IntcodeUtils where

import Common.Intcode
import qualified Data.Sequence as Seq

wrapMemory :: [Int] -> [Int] -> Memory
wrapMemory regs inputs' = Mem 0 (Seq.fromList regs) inputs' [] 0