module AoC12.NBodySpec (spec) where

import AoC12.NBody
import Test.Hspec
import Linear.V3

spec :: Spec
spec = do
    describe "Calculating velocities" $ do
        it "can calculate velocity between two moons" $ do
            calcVelocity moon1 moon2 `shouldBe` V3 1 (-1) (-1)
            calcVelocity moon2 moon3 `shouldBe` V3 1 1 1
            calcVelocity expectedMoon1 expectedMoon2 `shouldBe` V3 1 (-1) (-1)
            
        it "can calculate velecities for all moons in a system" $ do
            step sys `shouldBe` expectedSys
            step expectedSys `shouldBe` sys2
        
        it "can calculate and add multiple velocities" $ do
            calcVelocities moon1 sys `shouldBe` V3 3 (-1) (-1)
            calcVelocities expectedMoon1 expectedSys `shouldBe` V3 3 (-2) (-2)
    
    describe "Energy" $
        it "can calculate the energy of a system" $
            energy sys10 `shouldBe` 179
    
    describe "Iterate and calculate energy" $ 
        it "can iterate n times and get the energy" $ 
            totalEnergy 10 sys `shouldBe` 179


moon1 = Moon (V3 (-1) 0     2) (V3 0 0 0)
moon2 = Moon (V3 2 (-10) (-7)) (V3 0 0 0)
moon3 = Moon (V3 4 (-8)  8) (V3 0 0 0)
moon4 = Moon (V3 3   5  (-1))  (V3 0 0 0)

sys = [moon1, moon2, moon3, moon4]

expectedMoon1 = Moon (V3 2 (-1) ( 1)) (V3 3 (-1) (-1))
expectedMoon2 = Moon (V3 3 (-7) (-4)) (V3 1 ( 3) ( 3))
expectedMoon3 = Moon (V3 1 (-7) ( 5)) (V3 (-3) ( 1) (-3))
expectedMoon4 = Moon (V3 2 ( 2) ( 0)) (V3 (-1) (-3) ( 1))

expectedSys = [expectedMoon1, expectedMoon2, expectedMoon3, expectedMoon4]

finalMoon1 = Moon (V3 2 ( 1) (-3)) (V3 (-3) (-2) ( 1))
finalMoon2 = Moon (V3 1 (-8) ( 0)) (V3 (-1) ( 1) ( 3))
finalMoon3 = Moon (V3 3 (-6) ( 1)) (V3 ( 3) ( 2) (-3))
finalMoon4 = Moon (V3 2 ( 0) ( 4)) (V3 ( 1) (-1) (-1))
sys10 = [finalMoon1, finalMoon2, finalMoon3, finalMoon4]


secondExpectedMoon1 = Moon (V3 (5)(-3)(-1)) (V3 ( 3)(-2)(-2))
secondExpectedMoon2 = Moon (V3 (1)(-2)( 2)) (V3 (-2)( 5)( 6))
secondExpectedMoon3 = Moon (V3 (1)(-4)(-1)) (V3 ( 0)( 3)(-6))
secondExpectedMoon4 = Moon (V3 (1)(-4)( 2)) (V3 (-1)(-6)( 2))
sys2 = [secondExpectedMoon1, secondExpectedMoon2, secondExpectedMoon3, secondExpectedMoon4]