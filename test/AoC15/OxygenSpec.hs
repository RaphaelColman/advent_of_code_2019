module AoC15.OxygenSpec (spec) where

import AoC15.Oxygen
import Test.Hspec

spec :: Spec
spec = 
    describe "simplifying directions" $
        it "should simplify unit vectors" $ do
            simplifyCardinals directions `shouldBe` [South]
            simplifyCardinals [North, South] `shouldBe` []
            simplifyCardinals [North, North, North, West] `shouldBe` [North, North, North, West]
            simplifyCardinals [North, North, North, West, East, West] `shouldBe` [North, North, North, West]

directions = [West, West, East, North, South, East, South]