{-# LANGUAGE TemplateHaskell #-}
module AoC14.SpaceFuel where

import           Common.Utils
import           Control.Lens.TH
import           Data.Char
import           Data.List.Split
import           Data.Map        (Map (..))
import qualified Data.Map        as Map
import           Data.Tuple
import           System.IO

type Chemical = String
type Ingredient = (Integer, Chemical)
data Reaction = Reaction {
    _output :: Ingredient,
    _inputs :: [Ingredient]
} deriving (Eq, Show)
type ReactionList = [Reaction]
type Instruction = (Integer, [Ingredient])

data Refinery = Refinery {
    _reserves :: Map Chemical Integer,
    _required :: Map Chemical Integer
} deriving (Eq, Show)

type Specification = Map Chemical Instruction

makeLenses ''Reaction
makeLenses ''Refinery

main :: IO ()
main = do
    fileHandle <- openFile "src/AoC14/input.txt" ReadMode
    contents <- hGetContents fileHandle
    print $ calculateOre 1 $ makeReactionMap $ parseReactionList contents
    print $ fuelFromOre 1000000000000 $ makeReactionMap $ parseReactionList contents

parseReaction :: String -> Reaction
parseReaction str = Reaction output' inputs'
    where [in', out'] = splitOn " => " str
          inputs' = map (makeInput . words) $ splitOn ", " in'
          makeInput [num, chem] = (read num, chem) --use readMaybe?
          makeInput _           = error "Error parsing input"
          output' = (makeInput . words) out'

parseReactionList :: String -> ReactionList
parseReactionList = map parseReaction . lines

makeReactionMap :: ReactionList -> Specification
makeReactionMap = Map.fromList . map toTup
    where toTup (Reaction o i) = (snd o, (fst o, i))


calculateOre :: Integer -> Specification -> Integer
calculateOre amount spec = calculateOreImpl spec (Refinery Map.empty (Map.fromList [("FUEL", amount)]))

calculateOreImpl :: Specification -> Refinery -> Integer
calculateOreImpl spec ref@(Refinery _ required')
    | null required' = 0
    | onlyOre required' = Map.foldr (+) 0 required'
    | otherwise = calculateOreImpl spec (synthesise adjustedForReserves spec)
        where adjustedForReserves = takeFromReserves ref

multiplyIngredient :: Integer -> Ingredient -> Ingredient
multiplyIngredient a (x, chem) = (a*x, chem)

onlyOre :: Map Chemical Integer -> Bool
onlyOre = not . any (/="ORE") . Map.keys

takeFromReserves :: Refinery -> Refinery
takeFromReserves (Refinery reserves' required') = Refinery newReserves newRequired
    where newReserves = removeNotNeeded $ Map.unionWith (-) reserves' amountToSubtract
          newRequired = removeNotNeeded $ Map.unionWith (-) required' amountToSubtract
          amountToSubtract = Map.intersectionWith min reserves' required'
          removeNotNeeded = Map.filter (/=0)

synthesise :: Refinery -> Specification -> Refinery
synthesise (Refinery reserves' required') spec = Refinery newReserves newRequired
    where reactions = Map.mapMaybeWithKey (\k a -> getReaction k a spec) required'
          newReserves = addIngredientsToMap (map _output $ Map.elems reactions) reserves'
          newRequired = addIngredientsToMap (concatMap _inputs (Map.elems reactions)) required'

getReaction :: Chemical -> Integer -> Specification -> Maybe Reaction
getReaction chem amount spec = do
    (producedAmount, ingredients) <- Map.lookup chem spec --This will return Nothing if you look up ORE
    let factor = ceiling $ (fromInteger amount :: Double) / (fromInteger producedAmount :: Double)
    Just $ Reaction (factor * producedAmount, chem) (map (multiplyIngredient factor) ingredients)

addIngredientsToMap :: [Ingredient] -> Map Chemical Integer -> Map Chemical Integer
addIngredientsToMap ings mp = let ingredientMap = Map.fromListWith (+) (map swap ings) in
                               Map.unionWith (+) ingredientMap mp

fuelFromOre :: Integer -> Specification -> Maybe Integer
fuelFromOre i spec = binarySearch i (`calculateOre` spec) 0 (lowerBound*2)
    where oreForSingleFuel = calculateOre 1 spec
          lowerBound = oreForSingleFuel * i

binarySearch :: Integer -> (Integer -> Integer) -> Integer -> Integer -> Maybe Integer
binarySearch value f lower upper
    | lower >= upper = Nothing
    | abs (upper - lower) == 1 = Just target
    | f target == value = Just target
    | f target > value = binarySearch value f lower target
    | f target < value = binarySearch value f target upper
    | otherwise = Nothing
        where target = (upper + lower) `div` 2
