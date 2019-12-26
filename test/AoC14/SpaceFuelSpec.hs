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
            --calculateOreIO 1 `shouldReturn` 31
            --calculateOreIO 2 `shouldReturn` 165
            calculateOreIO 3 `shouldReturn` 13312
    
    describe "Synthesising materials" $ do
        it "should put materials in reserves" $
            refineIO 1 `shouldReturn` expectedRef1
        
        it "should take from reserves" $
            takeFromReserves refineryWithReserves `shouldBe` refineryAfterTakingFromReserves
 
calculateOreIO :: Int -> IO Int
calculateOreIO i = do
    fileHandle <- openFile ("test/AoC14/input" ++ show i ++ ".txt") ReadMode
    contents <- hGetContents fileHandle
    pure $ calculateOre $ makeReactionMap $ parseReactionList contents

ref1 = Refinery Map.empty $ Map.fromList [("A", 7), ("E", 1)]
expectedRef1 = Refinery (Map.fromList [("A", 10), ("E", 1)]) $ Map.fromList [("A",14),("D",1),("E",1),("ORE",10)]

refineIO :: Int -> IO Refinery
refineIO i = do
    fileHandle <- openFile ("test/AoC14/input" ++ show i ++ ".txt") ReadMode
    contents <- hGetContents fileHandle
    let spec = makeReactionMap $ parseReactionList contents
    return $ synthesise ref1 spec
    
refineryWithReserves = Refinery (Map.fromList [("A", 10), ("B", 9)]) (Map.fromList [("A", 5)])
refineryAfterTakingFromReserves = Refinery (Map.fromList [("A", 5), ("B", 9)]) Map.empty


needsARefinery = Refinery {_reserves = Map.fromList [], _required = Map.fromList [("AB",2),("BC",3),("CA",4)]}

needsAIO :: IO Refinery
needsAIO = do
    fileHandle <- openFile ("test/AoC14/input2.txt") ReadMode
    contents <- hGetContents fileHandle
    let spec = makeReactionMap $ parseReactionList contents
    return $ synthesise needsARefinery spec
