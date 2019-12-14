module AoC7.Amplification where

import Common.Intcode
import Data.List
import qualified Data.Sequence as Seq
import System.IO
import Data.List.Split

type PhaseSequence = [Int]

main :: IO ()
main = do
  handle <- openFile "src/AoC7/input.txt" ReadMode  
  contents <- hGetContents handle
  print $ findBestPhaseSequence $ parseToList contents

runWithPhaseSequence :: PhaseSequence -> [Int] -> Maybe Int
runWithPhaseSequence phaseSeq registers' = foldl' doRunIntcode (Just 0) phaseSeq
    where doRunIntcode :: Maybe Int -> Int -> Maybe Int
          doRunIntcode (Just output') phase = runIntCodeAndReadOutput (Mem 0 (Seq.fromList registers') [phase, output'] [])
          doRunIntcode Nothing _ = Nothing


runIntCodeAndReadOutput :: Memory -> Maybe Int
runIntCodeAndReadOutput mem = do
    newMem <- runIntCode mem
    pure $ head $ outputs newMem


findBestPhaseSequence :: [Int] -> Maybe Int
findBestPhaseSequence xs = let allPhaseSequences = permutations [0..4] in do
    outputs' <- traverse (`runWithPhaseSequence` xs) allPhaseSequences
    pure $ maximum outputs'

parseToList :: String -> [Int]
parseToList = map read . splitOn ","