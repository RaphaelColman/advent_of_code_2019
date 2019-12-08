{-# LANGUAGE LambdaCase                #-}
module Common.Intcode where

import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq

data Memory = Mem { position :: Int,
                    registers :: Seq Int,
                    inputs :: [Int], 
                    outputs :: [Int]
                  } deriving (Show, Eq)

step :: Memory -> Maybe Memory
step (Mem pos regs _ _) = do
  op <- Seq.lookup pos regs >>= parseOpcode
  locations <- let num = numParams op in traverse (flip Seq.lookup regs) [pos+1..pos+num+1]
  params <- traverse (flip Seq.lookup regs) locations
  --[o1, o2] <- sequence [Seq.lookup p1 regs, Seq.lookup p2 regs]
  pure $ Mem (pos+4) (Seq.update (last params) ((uncurry inst) params) regs) [] []
    where inst = (+) 
-- Looking up a variable number of params doesn't work because uncurry expects a tuple of fixed length.
-- We should just case based on the opCode


runIntCode :: Memory -> Maybe Memory
runIntCode m@(Mem pos regs _ _)
  | Seq.lookup pos regs ==  Just 99 = Just m 
  | otherwise = step m >>= runIntCode

opCode :: Num a => Int -> Maybe (a -> a -> a)
opCode i = case i of
             1 -> Just (+)
             2 -> Just (*)
             _ -> Nothing

data Instruction = Add | Multiply | In | Out deriving (Show, Eq, Enum, Ord)

data Opcode = Opcode { instruction :: Instruction ,
                      numParams :: Int 
                     } deriving (Show, Eq)

parseOpcode :: Int -> Maybe Opcode
parseOpcode = \case
            1 -> Just (Opcode Add 2)
            2 -> Just (Opcode Multiply 2)
            3 -> Just (Opcode In 1)
            4 -> Just (Opcode Out 1)
            _ -> Nothing
