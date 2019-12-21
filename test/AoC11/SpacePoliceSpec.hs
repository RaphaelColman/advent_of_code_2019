module AoC11.SpacePoliceSpec (spec) where

import qualified AoC11.SpacePolice as SP
import AoC11.SpacePolice
import Test.Hspec
import Linear.V2

spec :: Spec
spec = do
    describe "Turning" $
        it "faces correct direction after turning" $ do
            turn North TurnRight `shouldBe` East
            turn East TurnRight `shouldBe` South
            turn South TurnRight `shouldBe` West
            turn West TurnRight `shouldBe` North
            turn North TurnLeft `shouldBe` West
    
    describe "moving" $
        it "moves in the direction it is facing" $ do
            moveRobot (V2 0 0) North `shouldBe` V2 0 1
            moveRobot (V2 0 0) East `shouldBe` V2 1 0
            moveRobot (V2 0 0) South `shouldBe` V2 0 (-1)
            moveRobot (V2 0 0) West `shouldBe` V2 (-1) 0
    
    describe "colour int conversion" $
        it "gets colour for int" $ do
            colourForInt 0 `shouldBe` Black
            colourForInt 1 `shouldBe` White

    describe "turn int conversion" $
        it "gets turn for int" $ do
            turnForInt 0 `shouldBe` TurnLeft
            turnForInt 1 `shouldBe` TurnRight