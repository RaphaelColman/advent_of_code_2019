module AoC7.Amplification where

import Common.Intcode
import Data.List
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import System.IO
import Data.List.Split
import Common.Utils
import Data.Maybe

type PhaseSequence = [Int]

main :: IO ()
main = do
  handle <- openFile "src/AoC7/input.txt" ReadMode
  contents <- hGetContents handle
  print $ findBestPhaseSequence $ parseToList contents
  print $ findBestPhaseSequenceFeedbackLoop $ parseToList contents


data Amplifiers = Amplifiers {position :: Int, initialInput :: Int, amplifiers :: Seq Memory, phaseSequence :: [Int]} deriving (Eq, Show)

doAmplifiers :: PhaseSequence -> [Int] -> Maybe Int
doAmplifiers ps regs = let result = runAmplifiers (initAmplifiers ps regs) in
    result >>= getFinalOutput

getFinalOutput :: Amplifiers -> Maybe Int
getFinalOutput amps = do
    lastAmp <- seqLast (amplifiers amps)
    pure $ head $ outputs lastAmp

initAmplifiers :: PhaseSequence -> [Int] -> Amplifiers
initAmplifiers phaseSequence' registers' = Amplifiers 0 0 (Seq.fromList amplifiersWithPhaseInputAdded) phaseSequence'
    where allAmps = replicate (length phaseSequence') (wrapMemory registers')
          allAmpsWithInitialImput = appendInput (head allAmps) 0 : tail allAmps 
          amplifiersWithPhaseInputAdded = zipWith appendInput allAmpsWithInitialImput phaseSequence' 

-- we need to inject initial input somehow
-- which should be zipping phase and amplifiers'
stepToNextAmplifier :: Amplifiers -> Maybe Amplifiers
stepToNextAmplifier a@(Amplifiers position' initialInput' amplifiers' phase) = let nextPos = (position'+1) `mod` Seq.length amplifiers' in do
    amplifier <- Seq.lookup position' amplifiers'
    input' <- if isJust prevOutput then sequence [prevOutput] else Just []
    newAmplifier <- runIntCodeWithInput input' amplifier
    let newAmplifiers = Seq.update position' newAmplifier amplifiers' in
        pure $ Amplifiers nextPos initialInput' newAmplifiers phase
        where prevOutput = getPreviousOutput a

getPreviousOutput :: Amplifiers -> Maybe Int
getPreviousOutput (Amplifiers position' _ amplifiers' _) = do
    prevAmp <- Seq.lookup prevPos amplifiers';
    let out' = outputs prevAmp in
        if not (null out') then pure $ head out' else Nothing 
        where prevPos = (position'-1) `mod` Seq.length amplifiers'

runAmplifiers :: Amplifiers -> Maybe Amplifiers
runAmplifiers amps
    | halted amps = Just amps
    | otherwise = stepToNextAmplifier amps >>= runAmplifiers

halted :: Amplifiers -> Bool
halted amps = let lastAmp = seqLast (amplifiers amps) in
    case lastAmp of
        Just mem' -> intcodeHalted mem'
        _ -> True


runWithPhaseSequence :: PhaseSequence -> [Int] -> Maybe Int
runWithPhaseSequence phaseSeq registers' = foldl' doRunIntcode (Just 0) phaseSeq
    where doRunIntcode :: Maybe Int -> Int -> Maybe Int
          doRunIntcode (Just output') phase = runIntCodeAndReadOutput (Mem 0 (Seq.fromList registers') [phase, output'] [] 0)
          doRunIntcode Nothing _ = Nothing

runIntCodeAndReadOutput :: Memory -> Maybe Int
runIntCodeAndReadOutput mem = do
    newMem <- runIntCode mem
    pure $ head $ outputs newMem


findBestPhaseSequence :: [Int] -> Maybe Int
findBestPhaseSequence xs = let allPhaseSequences = permutations [0..4] in do
    outputs' <- traverse (`runWithPhaseSequence` xs) allPhaseSequences
    pure $ maximum outputs'

findBestPhaseSequenceFeedbackLoop :: [Int] -> Maybe Int
findBestPhaseSequenceFeedbackLoop xs = let allPhaseSequences = permutations [5..9] in do
    outputs' <- traverse (`doAmplifiers` xs) allPhaseSequences
    pure $ maximum outputs'

parseToList :: String -> [Int]
parseToList = map read . splitOn ","

wrapMemory :: [Int] -> Memory
wrapMemory registers' = Mem 0 (Seq.fromList registers') [] [] 0