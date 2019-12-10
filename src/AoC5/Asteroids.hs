module AoC5.Asteroids where

import System.IO
import Common.Intcode
import Debug.Trace

main :: IO ()
main = do
  handle <- openFile "src/AoC5/input.txt" ReadMode  
  contents <- hGetContents handle
  putStrLn $ show $ runIntCode $ addInput 1 $ parse contents


addInput :: Int -> Memory -> Memory
addInput i (Mem pos regs input out) = Mem pos regs (i:input) out
