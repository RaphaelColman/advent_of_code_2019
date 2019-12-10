{-# LANGUAGE LambdaCase                #-}
module Common.Intcode where

import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq
import Data.List.Split
import Debug.Trace

data Memory = Mem { position :: Int,
                    registers :: Seq Int,
                    inputs :: [Int], 
                    outputs :: [Int]
                  } deriving (Show, Eq)

data Instruction = Add | Multiply | In | Out | Halt deriving (Show, Eq, Enum, Ord)
data Mode = Position | Immediate deriving (Show, Eq, Enum, Ord)
data Opcode = Opcode { instruction :: Instruction,
                      modes :: [Mode]
                     } deriving (Show, Eq)

parse :: String -> Memory
parse str = (Mem 0 (Seq.fromList intList) [] [])
  where intList = map read $ splitOn "," str :: [Int]

step :: Memory -> Maybe Memory
step m@(Mem pos regs _ _) = do
  trace ("Memory: " ++ show m) (Just (id m))
  op <- Seq.lookup pos regs >>= parseOpcode
  case (instruction op) of
    Add -> add m op
    Multiply -> multiply m op
    In -> readIn m
    Out -> output m op
    Halt -> pure m

add :: Memory -> Opcode -> Maybe Memory
add mem op = threeParamOperation (+) mem op

multiply :: Memory -> Opcode -> Maybe Memory
multiply mem op = threeParamOperation (*) mem op -- Make (*)/product the last parameter so we can have pointfree

--Instead of taking three params, use the sum and product functions which take a list. You can use the length of the 
--list to read the last param
threeParamOperation :: (Int -> Int -> Int) -> Memory -> Opcode -> Maybe Memory
threeParamOperation f (Mem pos regs input out) op = do
  [p1, p2, p3] <- traverse (flip Seq.lookup regs) [pos+1..pos+3]
  [a1, a2, _] <- sequence $ zipWith getForMode (modes op) [p1, p2, p3] --Don't need p3
  pure $ Mem (pos+4) (Seq.update p3 (f a1 a2) regs) input out
    where getForMode mode p = case mode of
                                Immediate -> Just p
                                Position -> Seq.lookup p regs

readIn :: Memory -> Maybe Memory
readIn (Mem pos regs input out) = do
  value <- pure $ head input
  writeTo <- Seq.lookup (pos+1) regs
  pure $ Mem (pos+2) (Seq.update writeTo value regs) input out

output :: Memory -> Opcode -> Maybe Memory
output m@(Mem pos regs input out) op = do
  value <- outputForMode m op
  pure $ Mem (pos+2) regs input (value : out)

outputForMode :: Memory -> Opcode -> Maybe Int
outputForMode (Mem pos regs _ _) (Opcode _ modes') = do
  param <- Seq.lookup (pos+1) regs
  case head modes' of
    Immediate -> Just param
    Position -> Seq.lookup param regs

runIntCode :: Memory -> Maybe Memory
runIntCode m@(Mem pos regs _ _)
  | Seq.lookup pos regs ==  Just 99 = Just m 
  | otherwise = trace ("m: " ++ show m) (step m) >>= runIntCode


parseInstruction :: Int -> Maybe Instruction
parseInstruction = \case
            1 -> Just Add
            2 -> Just Multiply
            3 -> Just In
            4 -> Just Out
            _ -> Nothing

parseOpcode :: Int -> Maybe Opcode
parseOpcode code = do
  instr <- parseInstruction i
  modes' <- parseModes m
  pure $ Opcode instr modes'
  where codeStr = show code
        split' = length codeStr - 2
        i = read (drop split' codeStr) :: Int
        m = take split' codeStr


parseModes :: String -> Maybe [Mode]
parseModes = sequence 
          . padList 3 (Just Position)
          . map parseMode
          . reverse


parseMode :: Char -> Maybe Mode
parseMode m = case m of
                '0' -> Just Position
                '1' -> Just Immediate
                _ -> Nothing

padList :: Int -> a -> [a] -> [a]
padList i val xs = let numExtra = (i - length xs) in
                       xs ++ (replicate numExtra val) 
