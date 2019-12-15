module AoC7.Amplification where

import Common.Intcode
import Data.List
import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq
import System.IO
import Data.List.Split

type PhaseSequence = [Int]

main :: IO ()
main = do
  handle <- openFile "src/AoC7/input.txt" ReadMode  
  contents <- hGetContents handle
  print $ findBestPhaseSequence $ parseToList contents


data Amplifiers = Amplifiers {position :: Int, initialInput :: Int, amplifiers :: Seq Memory, phaseSequence :: [Int]}

initAmplifiers :: PhaseSequence -> [Int] -> Amplifiers
initAmplifiers phaseSequence' registers' = Amplifiers 0 0 (Seq.fromList (replicate 5 (wrapMemory registers'))) phaseSequence'

--Problems: output needs to override previous output
-- we need to inject initial input somehow
stepToNextAmplifier :: Amplifiers -> Maybe Amplifiers
stepToNextAmplifier a@(Amplifiers position' initialInput' amplifiers' phase) = let nextPos = ((position'+1) `mod` Seq.length amplifiers') in do 
    amplifier <- Seq.lookup position' amplifiers'
    input' <- sequence [Just (phase !! nextPos), getPreviousOutput a]
    newAmplifier <- runIntCodeWithInput input' amplifier
    let newAmplifiers = Seq.update position' newAmplifier amplifiers' in
        pure $ Amplifiers nextPos initialInput' newAmplifiers phase

getPreviousOutput :: Amplifiers -> Maybe Int
getPreviousOutput (Amplifiers position' _ amplifiers' _) = do
    amp <- Seq.lookup ((position' -1) `mod` Seq.length amplifiers') amplifiers';
    let out' = outputs amp in
        pure $ head out' --this should not be a list


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

wrapMemory :: [Int] -> Memory
wrapMemory registers' = Mem 0 (Seq.fromList registers') [] []