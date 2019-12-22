{-# LANGUAGE TemplateHaskell #-}
module AoC13.CarePackage where

import Common.Intcode
import System.IO
import Linear.V2
import Control.Lens hiding (Empty)
import Control.Lens.TH
import qualified Data.Map as Map
import Data.List.Split
import Debug.Trace


type Point = V2 Int
data TileType = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq, Enum, Bounded)
data Tile = Tile {
    _tileType :: TileType,
    _position :: Point
} deriving (Show, Eq)

makeLenses ''Tile

main :: IO ()
main = do
    fileHandle <- openFile "src/AoC13/input.txt" ReadMode
    contents <- hGetContents fileHandle
    let outputs' =  reverse . outputs <$> runIntCode (parse contents)
    print $ numBlockTiles <$> (outputs' >>= parseOutput)

tileForInt :: Int -> Maybe TileType
tileForInt = flip Map.lookup tileMap
    where tileMap = Map.fromList $ zip [0..] (enumFrom Empty)

parseOutput :: [Int] -> Maybe [Tile]
parseOutput xs = mapM makeTile chunks
    where chunks = chunksOf 3 xs
          makeTile [a, b, c] = do
            tileType' <- tileForInt c
            pure $ Tile tileType' (V2 a b)
          makeTile _ = Nothing

numBlockTiles :: [Tile] -> Int
numBlockTiles = length . filter isBlock
    where isBlock t = t ^. tileType == Block