{-# LANGUAGE TemplateHaskell #-}

module AoC11.SpacePolice where

import Common.Intcode
import Linear.V2
import Linear.Vector
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence(Seq(..))
import Control.Lens hiding (element)
import Control.Lens.TH
import System.IO
import Common.Utils
import qualified Data.Map as Map




type Point = V2 Int
type PaintOp = (Point, Colour) 
data Direction = North | East | South | West deriving (Eq, Show, Enum, Bounded)
data Turn = TurnLeft | TurnRight deriving (Eq, Show, Enum, Bounded)
data Colour = Black | White deriving (Eq, Show, Enum, Bounded)
data System = System {
                        _rPosition :: V2 Int,
                        _rFacing :: Direction,
                        _rMemory :: Memory,
                        _paintOps :: [PaintOp] --This will be in reverse order for maximum laziness
                     } deriving (Eq, Show)

makeLenses ''System
makeLenses ''Direction
makeLenses ''Colour
makeLenses ''Memory

main :: IO ()
main = do
    handle <- openFile "src/AoC11/input.txt" ReadMode
    contents <- hGetContents handle
    let sys = initSystemFromString contents
    print $ totalPanelsPainted <$> paintPanels sys


paintPanels :: System -> Maybe System
paintPanels sys
    | intcodeHalted (sys ^. rMemory) = Just sys
    | otherwise = rStep sys >>= paintPanels

rStep :: System -> Maybe System
rStep sys@(System pos facing mem' paintHistory) = do
    let c = currentColour sys
    let input = if c == White then 1 else 0
    newMem <-runIntCodeWithInput [input] mem'
    let turnInt:colourInt:_ = outputs newMem --use the latest two outputs
    let newPaintOp = (pos, colourForInt colourInt)
    let newFacing = turn facing (turnForInt turnInt)
    let newPos = moveRobot pos newFacing
    pure $ System newPos newFacing newMem (newPaintOp : paintHistory)

--'apply' all the colours in the history to your panel
currentColour :: System -> Colour
currentColour sys = foldr (\po' _ -> snd po') Black $ filter ((==pos) . fst) po
    where po = sys ^. paintOps
          pos = sys ^. rPosition

turn :: Direction -> Turn -> Direction
turn d t =
    if t == TurnRight then enumNext d
    else enumPrev d


moveRobot :: V2 Int -> Direction -> V2 Int
moveRobot pos d = case d of 
    North -> pos + unit _y
    East -> pos + unit _x
    South -> pos + negated unit _y
    West -> pos + negated unit _x

colourForInt :: Int -> Colour
colourForInt i = if i == 1 then White else Black

turnForInt :: Int -> Turn
turnForInt i = if i == 1 then TurnRight else TurnLeft


initSystemFromString :: String -> System
initSystemFromString str = System (V2 0 0) North mem []
    where mem = parse str

totalPanelsPainted :: System -> Int
totalPanelsPainted = length . Set.fromList . map fst . view paintOps