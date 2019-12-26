{-# LANGUAGE TemplateHaskell #-}
module AoC14.SpaceFuel where

import Data.Tree
import Common.Utils
import Data.Char
import Data.List.Split
import Control.Lens.TH
import qualified Data.Map as Map
import Data.Map(Map(..))
import System.IO
import Data.Maybe
import Data.Tuple
import System.IO

type Chemical = String
type Ingredient = (Int, Chemical)
data Reaction = Reaction {
    _output :: Ingredient,
    _inputs :: [Ingredient]
} deriving (Eq, Show)
type ReactionList = [Reaction]
type Instruction = (Int, [Ingredient])

data Refinery = Refinery {
    _reserves :: Map Chemical Int,
    _required :: Map Chemical Int
} deriving (Eq, Show)

makeLenses ''Reaction
makeLenses ''Refinery

main :: IO ()
main = do
    fileHandle <- openFile "src/AoC14/input.txt" ReadMode
    contents <- hGetContents fileHandle
    print $ calculateOre $ makeReactionMap $ parseReactionList contents

parseReaction :: String -> Reaction
parseReaction str = Reaction output' inputs' 
    where [in', out'] = splitOn " => " str 
          inputs' = map (makeInput . words) $ splitOn ", " in'
          makeInput [num, chem] = (read num, chem) --use readMaybe?
          makeInput _ = error "Error parsing input"
          output' = (makeInput . words) out'

parseReactionList :: String -> ReactionList
parseReactionList = map parseReaction . lines

makeReactionMap :: ReactionList -> Map Chemical Instruction
makeReactionMap = Map.fromList . map toTup 
    where toTup (Reaction o i) = (snd o, (fst o, i))


calculateOre :: Map Chemical Instruction -> Int
calculateOre spec = calculateOreImpl spec (Refinery Map.empty (Map.fromList [("FUEL", 1)]))

calculateOreImpl :: Map Chemical Instruction -> Refinery -> Int
calculateOreImpl spec ref@(Refinery reserves required)
    | null required = 0
    | onlyOre required = Map.foldr (+) 0 required
    | otherwise = calculateOreImpl spec (synthesise adjustedForReserves spec)
        where adjustedForReserves = takeFromReserves ref

multiplyIngredient :: Int -> Ingredient -> Ingredient
multiplyIngredient a (x, chem) = (a*x, chem)

onlyOre :: Map Chemical Int -> Bool
onlyOre = null . filter (/="ORE") . Map.keys

takeFromReserves :: Refinery -> Refinery
takeFromReserves (Refinery reserves' required') = Refinery newReserves newRequired
    where newReserves = removeNotNeeded $ Map.unionWith (-) reserves' amountToSubtract
          newRequired = removeNotNeeded $ Map.unionWith (-) required' amountToSubtract
          amountToSubtract = Map.intersectionWith min reserves' required'
          removeNotNeeded = Map.filter (/=0)

synthesise :: Refinery -> Map Chemical Instruction -> Refinery
synthesise (Refinery reserves' required') spec = Refinery newReserves newRequired
    where reactions = Map.mapWithKey getReaction $ Map.filterWithKey (\k _ -> k /="ORE") required'
          newReserves = addIngredientsToMap (map _output $ Map.elems reactions) reserves'
          newRequired = addIngredientsToMap (concatMap _inputs (Map.elems reactions)) required'
          getReaction chem amount = let Just (producedAmount, ingredients) = Map.lookup chem spec; --This will return Nothing if you look up ORE
                                                factor = ceiling $ (fromIntegral amount :: Double) / (fromIntegral producedAmount :: Double)
                                                in Reaction (factor * producedAmount, chem) (map (multiplyIngredient factor) ingredients)

addIngredientsToMap :: [Ingredient] -> Map Chemical Int -> Map Chemical Int
addIngredientsToMap ings mp = let ingredientMap = Map.fromListWith (+) (map swap ings) in
                               Map.unionWith (+) ingredientMap mp