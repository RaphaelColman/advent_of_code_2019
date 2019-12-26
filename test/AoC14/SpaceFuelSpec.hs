module AoC14.SpaceFuelSpec (spec) where

import Test.Hspec
import AoC14.SpaceFuel
import System.IO

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
    
    describe "Synthesising materials" $
        it "should put materials in reserves" $ do
            refineIO `shouldReturn` expectedRef1
 
calculateOreIO :: Int -> IO Int
calculateOreIO i = do
    fileHandle <- openFile ("test/AoC14/input" ++ show i ++ ".txt") ReadMode
    contents <- hGetContents fileHandle
    pure $ calculateOre $ makeReactionMap $ parseReactionList contents

ref1 = Refinery Map.empty Map.empty

refineIO :: Int -> IO Int
refineIO i = do
    fileHandle <- openFile ("test/AoC14/input" ++ show i ++ ".txt") ReadMode
    contents <- hGetContents fileHandle
    let spec = makeReactionMap $ parseReactionList contents
    print "hello world"