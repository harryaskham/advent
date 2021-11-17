module TwentyNineteen.Day14 where

import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Debug.Trace
import Util

type Chemical = String

type Quantity = Integer

data Reaction = Reaction [(Chemical, Quantity)] (Chemical, Quantity) deriving (Show)

parseReaction :: String -> Reaction
parseReaction line = Reaction inputs' (outputChemical, read outputQuantity)
  where
    [inputLine, outputLine] = splitOn " => " line
    [outputQuantity, outputChemical] = words outputLine
    inputs = words <$> splitOn ", " inputLine
    inputs' = (\[q, c] -> (c, read q)) <$> inputs

-- Create map from chemical to the resources it needs and its yield
reactionMap :: [Reaction] -> M.Map Chemical Reaction
reactionMap = foldl' (\acc r@(Reaction _ (c, _)) -> M.insert c r acc) M.empty

ore :: M.Map Chemical Reaction -> [(Chemical, Quantity)] -> M.Map Chemical Quantity -> M.Map Chemical Quantity
ore _ [] haves = haves
ore rm (("ORE", q) : needs) haves = ore rm needs $ M.insertWith (+) "ORE" q haves
ore rm ((c, q) : needs) haves = ore rm (needs ++ multipliedReqs) newHaves
  where
    (Reaction requirements (_, outputQ)) = unjust $ M.lookup c rm
    alreadyHave = fromMaybe 0 $ M.lookup c haves
    additionalNeed = q - alreadyHave
    multiplier = ceiling (fromIntegral additionalNeed / fromIntegral outputQ)
    multipliedReqs = (fmap . fmap) (* multiplier) requirements
    actualOutput = multiplier * outputQ
    surplus = actualOutput - q
    newHaves = M.insertWith (+) c surplus haves

oreNeeded :: M.Map Chemical Reaction -> Quantity -> Quantity
oreNeeded rm fuel = unjust $ M.lookup "ORE" $ ore rm [("FUEL", fuel)] M.empty

search :: M.Map Chemical Reaction -> Quantity -> Quantity -> Quantity
search rm lower upper
  | lower == upper - 1 = lower
  | oreNeeded rm midpoint > 1000000000000 = search rm lower midpoint
  | otherwise = search rm midpoint upper
  where
    midpoint = (lower + upper) `div` 2

part1 :: IO (Maybe Quantity)
part1 = do
  ls <- lines <$> input 2019 14
  let reactions = parseReaction <$> ls
      rMap = reactionMap reactions
  return $ M.lookup "ORE" $ ore rMap [("FUEL", 1)] M.empty

part2 :: IO Quantity
part2 = do
  ls <- lines <$> input 2019 14
  let reactions = parseReaction <$> ls
      rMap = reactionMap reactions
  return $ search rMap 1 10000000000
