module AoC5.Asteroids where

import System.IO
import Common.Intcode

main :: IO ()
main = do
  handle <- openFile "src/AoC5/input.txt" ReadMode  
  contents <- hGetContents handle
  print $ runIntCode $ addInput 1 $ parse contents
  print $ runIntCode $ addInput 5 $ parse contents


addInput :: Int -> Memory -> Memory
addInput i (Mem pos regs input out) = Mem pos regs (i:input) out
