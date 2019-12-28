module AoC14.SpaceFuelSpec (spec) where

import Test.Hspec
import AoC14.SpaceFuel
import System.IO
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Parsing inputs" $
        it "Should parse reaction" $ do
            parseReaction "7 A, 1 B => 1 C" `shouldBe` Reaction (1, "C") [(7, "A"), (1, "B")]
            parseReaction "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL" `shouldBe` Reaction (1, "FUEL") [(44, "XJWVT"), (5, "KHKGT"), (1, "QDVJ"), (29, "NZVS"), (9, "GPVTF"), (48, "HKGWZ")]
    
    describe "Calculating ORE" $
        it "should calculate ore for fuel" $ do
            calculateOreIO 1 `shouldReturn` 31
            calculateOreIO 2 `shouldReturn` 165
            calculateOreIO 3 `shouldReturn` 13312
            calculateOreIO 4 `shouldReturn` 180697
            calculateOreIO 5 `shouldReturn` 2210736
    
    describe "Part 2" $
        it "should calculate fuel from ore" $ do
            fuelFromOreIO 3 `shouldReturn` Just 82892753
            fuelFromOreIO 4 `shouldReturn` Just 5586022
            fuelFromOreIO 5 `shouldReturn` Just 460664
    
calculateOreIO :: Int -> IO Integer
calculateOreIO i = do
    fileHandle <- openFile ("test/AoC14/input" ++ show i ++ ".txt") ReadMode
    contents <- hGetContents fileHandle
    pure $ calculateOre 1 $ makeReactionMap $ parseReactionList contents


fuelFromOreIO :: Int -> IO (Maybe Integer)
fuelFromOreIO i = do
    fileHandle <- openFile ("test/AoC14/input" ++ show i ++ ".txt") ReadMode
    contents <- hGetContents fileHandle
    pure $ fuelFromOre 1000000000000 $ makeReactionMap $ parseReactionList contents

minusNumberIO :: IO Integer
minusNumberIO = do
    fileHandle <- openFile "test/AoC14/input3.txt" ReadMode
    contents <- hGetContents fileHandle
    pure $ calculateOre 16640000000000000 $ makeReactionMap $ parseReactionList contents