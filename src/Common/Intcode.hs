{-# LANGUAGE LambdaCase                #-}
module Common.Intcode where

import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq
import Debug.Trace

data Memory = Mem { position :: Int,
                    registers :: Seq Int,
                    inputs :: [Int], 
                    outputs :: [Int]
                  } deriving (Show, Eq)

step :: Memory -> Maybe Memory
step m@(Mem pos regs _ _) = do
  op <- Seq.lookup pos regs >>= parseOpcode
  case op of
    Add -> add m
    Multiply -> multiply m
    In -> readIn m
    Out -> output m
    Halt -> pure $ m
-- Looking up a variable number of params doesn't work because uncurry expects a tuple of fixed length.
-- We should just case based on the opCode


add :: Memory -> Maybe Memory
add mem = threeParamOperation (+) mem

multiply :: Memory -> Maybe Memory
multiply mem = threeParamOperation (*) mem

threeParamOperation :: (Int -> Int -> Int) -> Memory -> Maybe Memory
threeParamOperation f (Mem pos regs input out) = do
  [p1, p2, p3] <- traverse (flip Seq.lookup regs) [pos+1..pos+3]
  [o1, o2] <- sequence [Seq.lookup p1 regs, Seq.lookup p2 regs]
  pure $ Mem (pos+4) (Seq.update p3 (f o1 o2) regs) input out

readIn :: Memory -> Maybe Memory
readIn (Mem pos regs input out) = do
  value <- pure $ head input
  writeTo <- Seq.lookup (pos+1) regs
  pure $ Mem (pos+2) (Seq.update writeTo value regs) input out

output :: Memory -> Maybe Memory
output (Mem pos regs input out) = do
  readFrom <- Seq.lookup (pos+1) regs
  value <- Seq.lookup readFrom regs
  newOutput <- pure $ value : out
  pure $ Mem (pos+2) regs input (out ++ newOutput)

runIntCode :: Memory -> Maybe Memory
runIntCode m@(Mem pos regs _ _)
  | Seq.lookup pos regs ==  Just 99 = Just m 
  | otherwise = step m >>= runIntCode

opCode :: Num a => Int -> Maybe (a -> a -> a)
opCode i = case i of
             1 -> Just (+)
             2 -> Just (*)
             _ -> Nothing

data Instruction = Add | Multiply | In | Out | Halt deriving (Show, Eq, Enum, Ord)

data Opcode = Opcode { instruction :: Instruction ,
                      numParams :: Int 
                     } deriving (Show, Eq)

parseOpcode :: Int -> Maybe Instruction
parseOpcode = \case
            1 -> Just Add
            2 -> Just Multiply
            3 -> Just In
            4 -> Just Out
            _ -> Nothing
