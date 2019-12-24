{-# LANGUAGE TemplateHaskell #-}
module AoC13.CarePackage where

import Common.Intcode
import System.IO
import Linear.V2
import Control.Lens hiding (Empty)
import Control.Lens.TH
import Data.List.Split
import qualified Data.Map as Map
import Data.Map(Map(..))
import Data.List
import Data.Maybe
import Common.Utils
import Data.Either

type Point = V2 Int
data TileType = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq, Enum, Bounded)
data Tile = Tile {
    _tileType :: TileType,
    _position :: Point
} deriving (Show, Eq)

type GameState = Map (V2 Int) TileType
data Game = Game {
    _gameState :: GameState,
    _memory :: Memory,
    _score :: Int
} deriving (Show, Eq)

makeLenses ''Tile
makeLenses ''Game

main :: IO ()
main = do
    fileHandle <- openFile "src/AoC13/input.txt" ReadMode
    contents <- hGetContents fileHandle
    let initialMem = parse contents
    print $ numBlockTilesFromMem initialMem
    let runOnce = runIntCode $ hackMemory initialMem
    --doGameInteractive $ fromJust $ runOnce >>= prepareGame
    print $ runOnce >>= prepareGame >>= playGame

tileForInt :: Int -> Maybe TileType
tileForInt = flip Map.lookup tileMap
    where tileMap = Map.fromList $ zip [0..] (enumFrom Empty)

parseOutput :: [Int] -> Maybe [Either Tile Int]
parseOutput xs = mapM makeTile chunks
    where chunks = chunksOf 3 xs
          makeTile [a, b, c] = if a == -1 && b == 0 then Just (Right c)
                else do
                    tileType' <- tileForInt c
                    pure $ Left $ Tile tileType' (V2 a b)
          makeTile e = error ("not a tile: " ++ show e)

numBlockTilesFromMem :: Memory -> Maybe Int
numBlockTilesFromMem mem' = do
    outputs' <- reverse . outputs <$> runIntCode mem'
    numBlockTiles <$> parseOutput outputs'

numBlockTiles :: [Either Tile Int] -> Int
numBlockTiles = length . filter isBlock
    where isBlock t = case t of
            Right _ -> False
            Left tile' -> tile' ^. tileType == Block

tilesToMap :: [Tile] -> Map (V2 Int) TileType
tilesToMap = Map.fromList . map tileToTup
    where tileToTup (Tile type' position') = (position', type')

gameStep :: Game -> Maybe Game
gameStep g@(Game gs mem' _) = do
    newTiles <- getGameState g
    let newGs = Map.union (newTiles ^. gameState) gs
    let input = decideInput newGs
    newMem <- runIntCodeWithInput [input] $ clearOutput mem'
    Just $ Game newGs newMem (newTiles ^. score)

getGameState :: Game -> Maybe Game
getGameState (Game _ mem' prevScore) = do
    tilesAndScore <- parseOutput $ (reverse . outputs) mem'
    let (tiles, scores) = partitionEithers tilesAndScore
    let score' = if not (null scores) then head scores else prevScore
    let tileMap = tilesToMap tiles
    pure $ Game tileMap mem' score'

decideInput :: GameState -> Int
decideInput gs
    | ballX > paddleX = 1
    | ballX < paddleX = -1
    | otherwise = 0
    where (ballPos, _):_ = Map.toList $ Map.filter (==Ball) gs
          (paddlePos, _):_ = Map.toList $ Map.filter (==Paddle) gs
          ballX = ballPos ^._x
          paddleX = paddlePos ^._x

hackMemory :: Memory -> Memory
hackMemory = writeToMemory 0 2

prepareGame :: Memory -> Maybe Game
prepareGame mem' = getGameState (Game Map.empty mem' 0)

playGame :: Game -> Maybe Int
playGame g@(Game gs _ _) = 
    evaluateGame gs
        where evaluateGame gs'
                | won gs' = Just (g ^. score)
                | lost gs' = Just (g ^. score)
                | otherwise = gameStep g >>= playGame

won :: GameState -> Bool
won = Map.null . Map.filter (== Block)

lost :: GameState -> Bool
lost = Map.null . Map.filter (== Ball)

renderGame :: Game -> String
renderGame = renderGameState . view gameState

renderGameState :: GameState -> String
renderGameState = renderVectorMap . Map.map typeToChar
    where typeToChar t = case t of
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

doGameInteractive :: Game -> IO Game
doGameInteractive game = do
    let newGame = fromJust $ gameStep game
    putStr $ renderGame newGame
    print "Enter to continue"
    _ <- getLine
    doGameInteractive newGame