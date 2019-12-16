module Common.UtilsSpec (spec) where

import Test.Hspec
import Common.Utils
import qualified Data.Sequence as Seq

spec :: Spec
spec = do
  describe "Updating and extending Sequence" $ do
    it "Can extend sequence with 0" $
        seqUpdateAndExtend 9 99 (Seq.fromList [1..5]) `shouldBe` Seq.fromList [1,2,3,4,5,0,0,0,0,99]
    
    it "does not extend when it does not need to" $
        seqUpdateAndExtend 2 99 (Seq.fromList [1..5]) `shouldBe` Seq.fromList [1,2,99,4,5]

  describe "Defaulting sequence lookups" $ do
    it "returns default value if sequence is too short" $
        seqLookupWithDefault 42 99 (Seq.fromList [1..5]) `shouldBe` 42

    it "returns looked up value if sequence is long enough" $
        seqLookupWithDefault 42 2 (Seq.fromList [1..5]) `shouldBe` 3 