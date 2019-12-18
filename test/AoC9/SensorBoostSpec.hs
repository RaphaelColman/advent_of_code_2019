module AoC9.SensorBoostSpec (spec) where

import Test.Hspec
import AoC9.SensorBoost
import Common.Intcode
import Common.IntcodeUtils
import qualified Data.Sequence as Seq
import System.IO
import Data.Maybe

spec :: Spec
spec = do
  describe "Relative mode features" $ do
    it "Can produce copy of itself" $ 
      outputs <$> runIntCode (wrapMemory [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] []) `shouldBe` Just (reverse [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

    it "Outputs 16-digit number" $
      outputs <$> runIntCode (wrapMemory [1102,34915192,34915192,7,4,7,99,0] []) `shouldBe` Just [1219070632396864]

    it "Outputs large number" $
      outputs <$> runIntCode (wrapMemory [104,1125899906842624,99] []) `shouldBe` Just [1125899906842624]
  
  describe "full system test" $
    it "can perform all ops in all modes" $
      ioRunIntcode `shouldReturn` Just [2351176124]
  

ioRunIntcode :: IO (Maybe [Int])
ioRunIntcode = do
  handle <- openFile "src/AoC9/input.txt" ReadMode
  contents <- hGetContents handle
  let mem = parse contents
  pure $ outputs <$> runIntCodeWithInput [1] mem
