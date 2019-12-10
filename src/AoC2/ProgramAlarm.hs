module AoC2.ProgramAlarm where

import Data.Sequence() 
import qualified Data.Sequence as Seq
import System.IO
import Common.Intcode

main :: IO ()
main = do
  handle <- openFile "src/AoC2/input.txt" ReadMode  
  contents <- hGetContents handle
  putStrLn $ show $ runIntCode (parse contents)
  putStrLn $ show $ findNounAndVerb (parse contents)

applyInputToMemory :: Input -> Memory -> Memory
applyInputToMemory (Input noun verb) (Mem pos reg _ _) = Mem pos (Seq.update 2 verb (Seq.update 1 noun reg)) [] []

findNounAndVerb :: Memory -> Maybe Memory 
findNounAndVerb mem = do
  allMemories <- pure $ map (flip applyInputToMemory mem) allInputs
  allResults <- traverse runIntCode allMemories
  pure $ head $ filter (\(Mem _ reg _ _) -> (Seq.lookup 0 reg) == Just 19690720) allResults
    where allInputs = [Input x y | x <- [1..99], y <- [1..99]]
  
type Noun = Int
type Verb = Int
data Input = Input Noun Verb deriving (Show, Eq)

