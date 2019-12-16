module AoC9.SensorBoostSpec (spec) where

import Test.Hspec
import AoC9.SensorBoost
import Common.Intcode
import Common.IntcodeUtils
import qualified Data.Sequence as Seq

spec :: Spec
spec = 
  describe "Relative mode features" $ do
    it "Can produce copy of itself" $
      registers <$> runIntCode (wrapMemory [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] []) `shouldBe` Just (Seq.fromList [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

    it "Outputs 16-digit number" $
      outputs <$> runIntCode (wrapMemory [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] []) `shouldBe` Just [9999999999999999]

    it "Outputs large number" $
      outputs <$> runIntCode (wrapMemory [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] []) `shouldBe` Just [1125899906842624]
