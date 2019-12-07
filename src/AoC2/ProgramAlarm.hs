module AoC2.ProgramAlarm where

import Data.List.Split
import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq
import System.IO

main :: IO ()
main = do
  handle <- openFile "src/AoC2/input.txt" ReadMode  
  contents <- hGetContents handle
  putStrLn $ show $ runIntCode (parse contents)
  putStrLn $ show $ findNounAndVerb (parse contents)

parse :: String -> Memory
parse str = (Mem 0 (Seq.fromList intList))
  where intList = map read $ splitOn "," str :: [Int]

data Memory = Mem { position :: Int, registers :: Seq Int} deriving (Show)

step :: Memory -> Maybe Memory
step (Mem pos regs) = do
  op <- Seq.lookup pos regs >>= opCode
  [p1, p2, p3] <- traverse (flip Seq.lookup regs) [pos+1..pos+3]
  [o1, o2] <- sequence [Seq.lookup p1 regs, Seq.lookup p2 regs]
  pure $ Mem (pos+4) (Seq.update p3 (op o1 o2) regs)


runIntCode :: Memory -> Maybe Memory
runIntCode m@(Mem pos regs)
  | Seq.lookup pos regs ==  Just 99 = Just m 
  | otherwise = step m >>= runIntCode

opCode :: Num a => Int -> Maybe (a -> a -> a)
opCode i = case i of
             1 -> Just (+)
             2 -> Just (*)
             _ -> Nothing


applyInputToMemory :: Input -> Memory -> Memory
applyInputToMemory (Input noun verb) (Mem pos reg) = Mem pos (Seq.update 2 verb (Seq.update 1 noun reg))

findNounAndVerb :: Memory -> Maybe Memory 
findNounAndVerb mem = do
  allMemories <- pure $ map (flip applyInputToMemory mem) allInputs
  allResults <- traverse runIntCode allMemories
  pure $ head $ filter (\(Mem _ reg) -> (Seq.lookup 0 reg) == Just 19690720) allResults
    where allInputs = [Input x y | x <- [1..99], y <- [1..99]]
  
type Noun = Int
type Verb = Int
data Input = Input Noun Verb deriving (Show, Eq)

