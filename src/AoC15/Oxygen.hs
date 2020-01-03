{-# LANGUAGE TemplateHaskell #-}
module AoC15.Oxygen where

import System.IO
import System.Random
import Common.Utils
import Common.Intcode
import Linear.V2
import qualified Data.Map as Map
import Data.Map(Map(..))
import Control.Lens
import Control.Lens.TH
import Linear.Vector
import Data.Maybe
import Debug.Trace
import qualified Data.Sequence as Seq
import Data.Sequence(Seq(..))
import Data.List
import Data.Graph
import qualified Data.Set as Set
import Data.Set(Set(..))

data Cardinal = North | South | West | East deriving (Show, Eq, Enum, Bounded, Ord)
data Tile = Fine | Wall | OxySystem deriving (Show, Eq, Enum, Bounded)
data RepairDroid = RepairDroid {
    _memory :: Memory,
    _environment :: Environment,
    _location :: Point,
    _randomGen :: StdGen,
    _travelHistory :: [Cardinal],
    _locationHistory :: Set Point
} deriving (Show)

type Environment = Map Point Tile
type Point = V2 Int

makeLenses ''RepairDroid


main :: IO ()
main = do
    fileHandle <- openFile "src/AoC15/input.txt" ReadMode
    contents <- hGetContents fileHandle
    gen <- getStdGen 
    let droid = initDroid (parse contents) gen
    exploreFullyIO droid
    --let done = fromJust $ exploreFully droid
    --putStr $ renderDroid done
    --print $ length $ simplifyCardinals $ reverse $ done ^. travelHistory

initDroid :: Memory -> StdGen -> RepairDroid
initDroid mem gen = RepairDroid mem (Map.fromList [(V2 0 0, Fine)]) (V2 0 0) gen [] Set.empty 

explore :: RepairDroid -> Maybe RepairDroid
explore rd
    | foundOxygenSystem (rd ^. memory) = Just rd
    | otherwise = moveRandom rd >>= explore

exploreFully :: RepairDroid -> Maybe RepairDroid
exploreFully rd
    | exploredAll rd = Just rd
    | otherwise = moveRandom rd >>= exploreFully

exploreFullyIO :: RepairDroid -> IO ()
exploreFullyIO rd
    | exploredAll rd = putStr $ renderDroid rd 
    | otherwise = do
        let nextMove = fromJust $ moveRandom rd 
        putStr $ renderDroid nextMove
        --getLine
        exploreFullyIO nextMove

isEdgeTile :: Point -> Environment -> Bool
isEdgeTile p env = not $ null unknown
    where possible = map (`moveDroid` p) $ enumFrom North
          unknown = filter (`Map.notMember` env) possible

exploredAll :: RepairDroid -> Bool
--exploredAll rd = trace ("nonWallEdgeTiles: " ++ show nonWallEdgeTiles ++ " environment: " ++ show env ++ " pos: " ++ show (rd ^. location)) $ not (null env) && all (\t -> Map.findWithDefault Fine t env == Wall) edgeTiles
exploredAll rd = not (null env) && all (\t -> Map.findWithDefault Fine t env == Wall) edgeTiles
    where edgeTiles = filter (`isEdgeTile` env) $ Map.keys env 
          env = rd ^. environment
          nonWallEdgeTiles = filter (\t -> Map.findWithDefault Fine t env /= Wall) edgeTiles

foundOxygenSystem :: Memory -> Bool
foundOxygenSystem mem
    | null (outputs mem) = False
    | otherwise = (==2) . head . outputs $ mem

tileForOutput :: Memory -> Tile
tileForOutput o = case head (outputs o) of
    0 -> Wall
    1 -> Fine --The word Empty is being used by another module
    2 -> OxySystem
    x -> error $ "Unexpected output from droid: " ++ show x --Use maybe instead?

moveRandom :: RepairDroid -> Maybe RepairDroid
moveRandom rd@(RepairDroid mem env loc gen history locHistory) = do
    --newMemory <- trace ("directionInt " ++ show randomDirectionInt) $ runIntCodeWithInput [randomDirectionInt] (clearOutput mem)
    let (newDirection, newGen) = decideDirection rd
    newMemory <- runIntCodeWithInput [intForCardinal newDirection] (clearOutput mem)
    let targetLocation = moveDroid newDirection loc
    let tile = tileForOutput newMemory
    let newEnv = Map.insert targetLocation tile env
    let newLocation = if tile /= Wall then targetLocation else loc
    let newTravelHistory = if tile /= Wall then newDirection:history else history
    let newLocHistory = if tile /= Wall then Set.insert loc locHistory else locHistory
    --pure $ trace ("rd: " ++ show rd) $ RepairDroid newMemory newEnv newLocation newGen
    pure $ RepairDroid newMemory newEnv newLocation newGen newTravelHistory newLocHistory

decideDirection :: RepairDroid -> (Cardinal, StdGen)
decideDirection (RepairDroid _ env loc gen _ locHistory) = (searchSpace !! randomInt , newGen)
    where possible = filter notAWall $ enumFrom North
          (randomInt, newGen) = randomR (0 :: Int, (length searchSpace - 1) :: Int) gen
          notAWall card = let newLoc = moveDroid card loc in Map.findWithDefault Fine newLoc env /= Wall
          visited card = Set.member (moveDroid card loc) locHistory
          newSpaces = filter (not . visited) possible
          searchSpace = if null newSpaces then possible else newSpaces


moveDroid :: Cardinal -> Point -> Point
moveDroid card loc = case card of
    North -> loc + unit _y
    South -> loc + negated unit _y
    West -> loc + negated unit _x
    East -> loc + unit _x

cardinalForInt :: Int -> Maybe Cardinal
cardinalForInt = flip Map.lookup cardMap
    where cardMap = Map.fromList $ zip [1..] $ enumFrom North

intForCardinal :: Cardinal -> Int 
intForCardinal c = fromJust $ Map.lookup c cardMap
    where cardMap = Map.fromList $ zip (enumFrom North) [1..]

unitVectorForCardinal :: Cardinal -> Point
unitVectorForCardinal c = case c of
    North -> unit _y
    South -> negated unit _y
    West ->  negated unit _x
    East ->  unit _x

renderDroid :: RepairDroid -> String
renderDroid rd = renderVectorMap plusLocation 
    where tileToChar k t
            | k == V2 0 0 = '0'
            | otherwise = case t of
                Fine -> '.'
                Wall -> '#'
                OxySystem -> '@'
          envMap = Map.mapWithKey tileToChar (rd ^. environment)
          plusLocation = Map.insert (rd ^. location) '^' envMap 

simplifyCardinals :: [Cardinal] -> [Cardinal]
simplifyCardinals ps = case firstPair of
                            Just (start, end) -> simplifyCardinals $ dropBetween start end ps
                            Nothing -> ps
    where uVectors = map unitVectorForCardinal ps
          visited = scanl (+) (V2 0 0) uVectors
          firstPair = firstPairIndex visited

firstPairIndex :: Eq a => [a] -> Maybe (Int, Int)
firstPairIndex = go 0
    where go :: Eq a => Int -> [a] -> Maybe (Int, Int)
          go i xs'
            | null newList = Nothing
            | length found < 2 = go (i+1) xs'
            | otherwise = Just (head found, found !! 1)
                where newList = drop i xs'
                      found = elemIndices (head newList) xs'

dropBetween :: Int -> Int -> [a] -> [a]
dropBetween start end xs = take start xs ++ drop end xs