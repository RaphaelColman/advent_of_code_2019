{-# LANGUAGE LambdaCase                #-}
module Common.Intcode where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.List.Split
import Control.Monad
import Common.Utils
import Debug.Trace

data Memory = Mem { position :: Int,
                    registers :: Seq Int,
                    inputs :: [Int],
                    outputs :: [Int],
                    relativeBase :: Int
                  } deriving (Show, Eq)

data Instruction = Add
                 | Multiply
                 | In
                 | Out
                 | JumpIfTrue
                 | JumpIfFalse
                 | LessThan
                 | Equal
                 | AdjustRelativeBase
                 | Halt deriving (Show, Eq, Enum, Ord)
data Mode = Position | Immediate | Relative deriving (Show, Eq, Enum, Ord)
data Opcode = Opcode { instruction :: Instruction,
                      modes :: [Mode]
                     } deriving (Show, Eq)

parse :: String -> Memory
parse str = Mem 0 (Seq.fromList intList) [] [] 0
  where intList = map read $ splitOn "," str :: [Int]

intcodeHalted :: Memory -> Bool
intcodeHalted (Mem pos regs _ _ _) = let op = instruction <$> (Seq.lookup pos regs >>= parseOpcode) in
  case op of
    Just Halt -> True
    _ -> False

awaitingInput :: Memory -> Bool
awaitingInput (Mem pos regs in' _ _) = let op = instruction <$> (Seq.lookup pos regs >>= parseOpcode) in
  case op of
    Just In -> null in'
    _ -> False

step :: Memory -> Maybe Memory
step m@(Mem pos regs _ _ _) = do
  op <- Seq.lookup pos regs >>= parseOpcode
  --op <- trace ("Memory: " ++ show m) Seq.lookup pos regs >>= parseOpcode
  case instruction op of
    Add -> add m op
    Multiply -> multiply m op
    In -> readIn m op
    Out -> writeToOutput m op
    JumpIfFalse -> jumpIfFalse m op
    JumpIfTrue -> jumpIfTrue m op
    LessThan -> lessThan m op
    Equal -> equal m op
    AdjustRelativeBase -> adjustRelativeBase m op
    Halt -> pure m

add :: Memory -> Opcode -> Maybe Memory
add = threeParamOperation (+)

multiply :: Memory -> Opcode -> Maybe Memory
multiply = threeParamOperation (*)
--Instead of taking three params, use the sum and product functions which take a list. You can use the length of the --list to read the last param
threeParamOperation :: (Int -> Int -> Int) -> Memory -> Opcode -> Maybe Memory
threeParamOperation f mem op = do
  [v1, v2] <- readNFromMemory 2 mem $ modes op
  let writePosition = readForWriting 3 mem $ last $ modes op
  let newMem = writeToMemory writePosition (f v1 v2) mem
  pure $ movePointer (pos + 4) newMem
    where pos = position mem

adjustRelativeBase :: Memory -> Opcode -> Maybe Memory
adjustRelativeBase mem@(Mem pos' regs' input' output' relBase) op = do
  x <- readNextFromMemory mem $ head $ modes op
  let newRelativeBase = relBase + x
  pure $ Mem (pos'+2) regs' input' output' newRelativeBase

readNFromMemory :: Int -> Memory -> [Mode] -> Maybe [Int]
readNFromMemory n (Mem pos regs _ _ relBase) modes' = do
  let positions = map (flip (seqLookupWithDefault 0) regs) [pos+1..pos+n]
  zipWithM getForMode modes' positions
    where getForMode mode p = case mode of
                                Immediate -> Just p
                                Position -> Just $ seqLookupWithDefault 0 p regs
                                Relative -> Just $ seqLookupWithDefault 0 (p + relBase) regs

readNextFromMemory :: Memory -> Mode -> Maybe Int
readNextFromMemory mem mode = head <$> readNFromMemory 1 mem [mode]

readForWriting :: Int -> Memory -> Mode -> Int
readForWriting shift (Mem pos regs _ _ relBase) mode = let value = seqLookupWithDefault 0 (pos+shift) regs in
      case mode of
        Position -> value
        Relative -> value + relBase
        Immediate -> value --Just assume it's position mode if we get this


writeToMemory :: Int -> Int -> Memory -> Memory
writeToMemory location value (Mem pos regs input out relativeBase') =
  Mem pos newRegs input out relativeBase'
    where newRegs = seqUpdateAndExtend location value regs

movePointer :: Int -> Memory -> Memory
movePointer location (Mem _ regs input out relBase) = Mem location regs input out relBase


readIn :: Memory -> Opcode -> Maybe Memory
readIn mem@(Mem pos regs input out relBase) op = let value = head input in do
  let writeTo = readForWriting 1 mem $ head $ modes op
  pure $ Mem (pos+2) (seqUpdateAndExtend writeTo value regs) (tail input) out relBase

writeToOutput :: Memory -> Opcode -> Maybe Memory
writeToOutput m@(Mem pos regs input out relBase) op = do
  value <- readNextFromMemory m $ head $ modes op
  pure $ Mem (pos+2) regs input (value : out) relBase


jumpIfFalse :: Memory -> Opcode -> Maybe Memory
jumpIfFalse = jumpOperation (== 0)

jumpIfTrue :: Memory -> Opcode -> Maybe Memory
jumpIfTrue = jumpOperation (/= 0)

jumpOperation :: (Int -> Bool) -> Memory -> Opcode -> Maybe Memory
jumpOperation f m@(Mem pos regs i o relBase) op = do
  [a1, a2] <- readNFromMemory 2 m $ modes op
  let newPointer = if f a1 then a2 else pos+3
  pure $ Mem newPointer regs i o relBase


lessThan :: Memory -> Opcode -> Maybe Memory
lessThan = threeParamOperation (\a b -> if a < b then 1 else 0)

equal :: Memory -> Opcode -> Maybe Memory
equal = threeParamOperation (\a b -> if a == b then 1 else 0)


runIntCode :: Memory -> Maybe Memory
runIntCode m@(Mem pos regs _ _ _)
  | Seq.lookup pos regs ==  Just 99 = Just m
  | awaitingInput m = Just m
  | otherwise = step m >>= runIntCode

runIntCodeWithInput :: [Int] -> Memory -> Maybe Memory
runIntCodeWithInput input' (Mem pos regs existingInput out' relBase) = let newMem = Mem pos regs (existingInput ++ input') out' relBase in
  runIntCode newMem

appendInput :: Memory -> Int -> Memory
appendInput (Mem pos regs in' out' relBase) xs = Mem pos regs (xs : in') out' relBase

parseInstruction :: Int -> Maybe Instruction
parseInstruction = \case
            1 -> Just Add
            2 -> Just Multiply
            3 -> Just In
            4 -> Just Out
            5 -> Just JumpIfTrue
            6 -> Just JumpIfFalse
            7 -> Just LessThan
            8 -> Just Equal
            9 -> Just AdjustRelativeBase
            99 -> Just Halt
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
                '2' -> Just Relative
                _ -> Nothing

padList :: Int -> a -> [a] -> [a]
padList i val xs = let numExtra = (i - length xs) in
                       xs ++ replicate numExtra val