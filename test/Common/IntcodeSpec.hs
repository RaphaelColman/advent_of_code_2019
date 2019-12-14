module Common.IntcodeSpec (spec) where

import Test.Hspec
import Common.Intcode
import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq

spec :: Spec
spec = do
  describe "Parsing opcode tests" $ do
    it "Can parse opcode" $ do
      parseInstruction 1 `shouldBe` Just Add
      parseInstruction 2 `shouldBe` Just Multiply
      parseInstruction 3 `shouldBe` Just In
      parseInstruction 4 `shouldBe` Just Out

    it "Can parse opcode with modes" $ do
      parseOpcode 1002 `shouldBe` Just (Opcode {instruction = Multiply, modes = [Position,Immediate,Position]})

  describe "Test new instruction types" $ do
    it "Can input and output" $ do
      runIntCode (Mem 0 (Seq.fromList [3,0,4,0,99]) [3] []) `shouldBe` Just (Mem 4 (Seq.fromList [3,0,4,0,99]) [] [3])
    
    it "Can perform operation with modes" $ do
      step multiplyWithModes `shouldBe` Just expectedResultFroMultiplyWithModes

multiplyWithModes :: Memory
multiplyWithModes = Mem 0 (Seq.fromList [1002, 4, 3, 4, 33]) [] []

expectedResultFroMultiplyWithModes :: Memory
expectedResultFroMultiplyWithModes = Mem 4 (Seq.fromList [1002, 4, 3, 4, 99]) [] []
