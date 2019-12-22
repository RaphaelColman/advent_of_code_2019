{-# LANGUAGE TemplateHaskell #-}
module AoC13.CarePackage where

import Common.Intcode
import System.IO
import Linear.V2
import Control.Lens hiding (Empty)
import Control.Lens.TH
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.List
import Data.Maybe

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
    let tiles = outputs' >>= parseOutput
    print $ numBlockTiles <$> (outputs' >>= parseOutput)
    putStr $ fromJust $ renderVectorMap <$> tilesToMap <$> tiles

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

tilesToMap :: [Tile] -> Map (V2 Int) Char
tilesToMap = Map.fromList . map tileToTup
    where tileToTup (Tile type' position') = (position', typeToChar type')
          typeToChar t = case t of
            Empty -> '.'
            Wall -> '+'
            Block -> '#'
            Paddle -> '_'
            Ball -> 'o'

renderVectorMap :: Map (V2 Int) Char -> String
renderVectorMap m = foo
    where keys = Map.keys m
          xMax = maximumBy (\a b -> compare (a ^._x) (b ^._x)) keys ^._x
          xMin = minimumBy (\a b -> compare (a ^._x) (b ^._x)) keys ^._x
          yMax = maximumBy (\a b -> compare (a ^._y) (b ^._y)) keys ^._y
          yMin = minimumBy (\a b -> compare (a ^._y) (b ^._y)) keys ^._y
          xRange = (xMax - xMin) + 1
          panelList = [Map.findWithDefault '.' (V2 x y) m | y <- [yMin .. yMax], x <- [xMin..xMax]]
          panelRows = chunksOf xRange panelList
          foo = unlines panelRows