module AoC12.NBodySpec (spec) where

import AoC12.NBody
import Test.Hspec
import System.IO
import Data.Maybe

spec :: Spec
spec = do
    describe "Iterate and calculate energy" $ 
        it "can iterate n times and get the energy" $ do
            totalEnergyIO 10 (testInput 1) `shouldReturn` 179 
            totalEnergyIO 100 (testInput 2) `shouldReturn` 1940
    
    describe "First repeated" $
        it "can find the first repeated system" $ do
            calculateFirstRepeat <$> testInput 1 `shouldReturn` 2772
            calculateFirstRepeat <$> testInput 2 `shouldReturn` 4686774924 


totalEnergyIO :: Int -> IO System -> IO Int
totalEnergyIO steps = fmap (totalEnergy steps)

testInput :: Int -> IO System
testInput i = do
    fileHandle <- openFile ("test/AoC12/input" ++ show i ++ ".txt") ReadMode
    contents <- hGetContents fileHandle
    pure $ fromJust $ parseSystem contents
