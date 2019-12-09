module Common.IntcodeSpec (spec) where

import Test.Hspec
import Common.Intcode
import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq

spec :: Spec
spec = do
  describe "Parsing opcode tests" $ do
    it "Can parse opcode" $ do
      parseOpcode 1 `shouldBe` Just Add
      parseOpcode 2 `shouldBe` Just Multiply
      parseOpcode 3 `shouldBe` Just In
      parseOpcode 4 `shouldBe` Just Out

  describe "Test new instruction types" $ do
    it "Can input and output" $ do
      runIntCode (Mem 0 (Seq.fromList [3,0,4,0,99]) [3] []) `shouldBe` Just (Mem 4 (Seq.fromList [3,0,4,0,99]) [3] [3])

