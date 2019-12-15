module AoC7.AmplificationSpec (spec) where

import Test.Hspec
import AoC7.Amplification


spec :: Spec
spec = 
  describe "output for phases" $ do
    it "can calculate output for phases" $ do
      runWithPhaseSequence [4,3,2,1,0] [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] `shouldBe` Just 43210
      runWithPhaseSequence [0,1,2,3,4] [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] `shouldBe` Just 54321
      runWithPhaseSequence [1,0,4,3,2] [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] `shouldBe` Just 65210
    --xit "can run in feedback loop" $
      --runInFeedbackMode [9,8,7,6,5] [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] `shouldBe` Just 139629729