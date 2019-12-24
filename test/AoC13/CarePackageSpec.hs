module AoC13.CarePackageSpec (spec) where

import Test.Hspec
import AoC13.CarePackage
import System.IO
import Common.Intcode

spec :: Spec
spec = 
    describe "Parsing output" $
        it "can count number of block tiles" $
            ioNumBlockTiles `shouldReturn` Just 228

ioNumBlockTiles :: IO (Maybe Int)
ioNumBlockTiles = do
    fileHandle <- openFile "src/AoC13/input.txt" ReadMode
    contents <- hGetContents fileHandle
    let outputs' =  reverse . outputs <$> runIntCode (parse contents)
    let tiles = outputs' >>= parseOutput
    pure $ numBlockTiles <$> (outputs' >>= parseOutput)