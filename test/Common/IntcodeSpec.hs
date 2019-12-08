module Common.IntcodeSpec (spec) where

import Test.Hspec
import Common.Intcode
import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq

spec :: Spec
spec = do
  describe "Parsing opcode tests" $ do
    it "Can parse opcode" $ do
      parseOpcode 1 `shouldBe` Just (Opcode Add 2)
      parseOpcode 2 `shouldBe` Just (Opcode Multiply 2)
      parseOpcode 3 `shouldBe` Just (Opcode In 1)
      parseOpcode 4 `shouldBe` Just (Opcode Out 1)

  describe "Test new instruction types" $ do
    xit "Can input and output" $ do
      step (Mem 0 (Seq.fromList [3,0,4,0,99]) [3] []) `shouldBe` Just (Mem 4 (Seq.fromList [3,0,4,0,99]) [] [42])

