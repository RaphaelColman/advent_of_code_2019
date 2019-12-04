module AoC2.ProgramAlarmSpec (spec) where

import Test.Hspec
import AoC2.ProgramAlarm

spec :: Spec
spec = do
  describe "Intcode general test" $ do
    it "Intcode should produce correct list" $ do
      intcode ([1,0,0,0,99] :: [Int]) `shouldBe` ([2,0,0,0,99] :: [Int])
      intcode ([2,3,0,3,99] :: [Int]) `shouldBe` ([2,3,0,6,99] :: [Int])
      intcode ([2,4,4,5,99,0] :: [Int]) `shouldBe` ([2,4,4,5,99,9801] :: [Int])
      intcode ([1,1,1,4,99,5,6,0,99] :: [Int]) `shouldBe` ([30,1,1,4,2,5,6,0,99] :: [Int])

    it "intcodeAtIndex should apply intcode" $ do
      intcodeAtIndex [1,1,1,4,2,5,6,0,99] 4 `shouldBe` [30,1,1,4,2,5,6,0,99]
