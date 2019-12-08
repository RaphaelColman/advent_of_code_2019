module AoC2.ProgramAlarmSpec (spec) where

import Test.Hspec
import AoC2.ProgramAlarm
import Common.Intcode
import Data.Sequence (Seq(..)) 
import qualified Data.Sequence as Seq

spec :: Spec
spec = do
  describe "Intcode general test" $ do
    it "Intcode should produce correct list" $ do
      readMemory (runIntCode (parse "1,0,0,0,99")) `shouldBe` Seq.fromList ([2,0,0,0,99])
      readMemory (runIntCode (parse "1,1,1,4,99,5,6,0,99")) `shouldBe` Seq.fromList ([30,1,1,4,2,5,6,0,99])


readMemory :: Maybe Memory -> Seq Int
readMemory (Just (Mem _ r _ _)) = r
readMemory Nothing = Seq.empty

