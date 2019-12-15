{-# LANGUAGE LambdaCase                #-}
module Common.Intcode where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.List.Split
import Control.Monad

data Memory = Mem { position :: Int,
                    registers :: Seq Int,
                    inputs :: [Int],
                    outputs :: [Int]
                  } deriving (Show, Eq)

data Instruction = Add
                 | Multiply
                 | In
                 | Out
                 | JumpIfTrue
                 | JumpIfFalse
                 | LessThan
                 | Equal
                 | Halt deriving (Show, Eq, Enum, Ord)
data Mode = Position | Immediate deriving (Show, Eq, Enum, Ord)
data Opcode = Opcode { instruction :: Instruction,
                      modes :: [Mode]
                     } deriving (Show, Eq)

parse :: String -> Memory
parse str = Mem 0 (Seq.fromList intList) [] []
  where intList = map read $ splitOn "," str :: [Int]

intcodeHalted :: Memory -> Bool
intcodeHalted (Mem pos regs _ _) = let op = instruction <$> (Seq.lookup pos regs >>= parseOpcode) in
  case op of
    Just Halt -> True
    _ -> False

step :: Memory -> Maybe Memory
step m@(Mem pos regs _ _) = do
  op <- Seq.lookup pos regs >>= parseOpcode
  case instruction op of
    Add -> add m op
    Multiply -> multiply m op
    In -> readIn m
    Out -> output m op
    JumpIfFalse -> jumpIfFalse m op
    JumpIfTrue -> jumpIfTrue m op
    LessThan -> lessThan m op
    Equal -> equal m op
    Halt -> pure m

add :: Memory -> Opcode -> Maybe Memory
add = threeParamOperation (+)

multiply :: Memory -> Opcode -> Maybe Memory
multiply = threeParamOperation (*)
--Instead of taking three params, use the sum and product functions which take a list. You can use the length of the --list to read the last param
threeParamOperation :: (Int -> Int -> Int) -> Memory -> Opcode -> Maybe Memory
threeParamOperation f mem op = do
  [v1, v2] <- readNFromMemory 2 mem $ modes op
  [_,_,outputLocation] <- readNFromMemoryImmediateMode 3 mem
  let newMem = writeToMemory outputLocation (f v1 v2) mem
  pure $ movePointer (pos + 4) newMem
    where pos = position mem


readNFromMemory :: Int -> Memory -> [Mode] -> Maybe [Int]
readNFromMemory n (Mem pos regs _ _) modes' = do
  positions <- traverse (`Seq.lookup` regs) [pos+1..pos+n]
  zipWithM getForMode modes' positions
    where getForMode mode p = case mode of
                                Immediate -> Just p
                                Position -> Seq.lookup p regs

readNextFromMemory :: Memory -> Mode -> Maybe Int 
readNextFromMemory mem mode = head <$> readNFromMemory 1 mem [mode]

readNFromMemoryImmediateMode :: Int -> Memory -> Maybe [Int]
readNFromMemoryImmediateMode n mem = readNFromMemory n mem $ repeat Immediate

writeToMemory :: Int -> Int -> Memory -> Memory
writeToMemory location value (Mem pos regs input out) =
  Mem pos newRegs input out
    where newRegs = Seq.update location value regs

movePointer :: Int -> Memory -> Memory
movePointer location (Mem _ regs input out) = Mem location regs input out


readIn :: Memory -> Maybe Memory
readIn (Mem pos regs input out) = let value = head input in do
  writeTo <- Seq.lookup (pos+1) regs
  pure $ Mem (pos+2) (Seq.update writeTo value regs) (tail input) out

output :: Memory -> Opcode -> Maybe Memory
output m@(Mem pos regs input out) op = do
  value <- readNextFromMemory m $ head $ modes op
  pure $ Mem (pos+2) regs input (value : out)


jumpIfFalse :: Memory -> Opcode -> Maybe Memory
jumpIfFalse = jumpOperation (== 0)

jumpIfTrue :: Memory -> Opcode -> Maybe Memory
jumpIfTrue = jumpOperation (/= 0)

jumpOperation :: (Int -> Bool) -> Memory -> Opcode -> Maybe Memory
jumpOperation f m@(Mem pos regs i o) op = do
  [a1, a2] <- readNFromMemory 2 m $ modes op
  let newPointer = if f a1 then a2 else pos+3
  pure $ Mem newPointer regs i o


lessThan :: Memory -> Opcode -> Maybe Memory
lessThan = threeParamOperation (\a b -> if a < b then 1 else 0)

equal :: Memory -> Opcode -> Maybe Memory
equal = threeParamOperation (\a b -> if a == b then 1 else 0)


runIntCode :: Memory -> Maybe Memory
runIntCode m@(Mem pos regs _ _)
  | Seq.lookup pos regs ==  Just 99 = Just m
  | otherwise = step m >>= runIntCode

--NB this overwrites existing inputs. Maybe it should add them?
runIntCodeWithInput :: [Int] -> Memory -> Maybe Memory
runIntCodeWithInput newInput' (Mem pos regs _ out') = let newMem = Mem pos regs newInput' out' in
  runIntCode newMem

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
                _ -> Nothing

padList :: Int -> a -> [a] -> [a]
padList i val xs = let numExtra = (i - length xs) in
                       xs ++ replicate numExtra val