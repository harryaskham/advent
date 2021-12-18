module Day18 where

import Data.Foldable (foldl1, maximum)
import Data.List (elemIndex, (!!))
import Data.List qualified as L
import Data.List.Extra (foldl1')
import Helper.TH (input)
import Helper.Util (number, parseLinesWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, sepBy)

type FishID = Integer

data Tree a
  = One a
  | Two (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Traversable Tree where
  traverse f t = case t of
    One x -> One <$> f x
    Two a b -> Two <$> traverse f a <*> traverse f b

type Fish = Tree (FishID, Int)

mkFish :: Int -> Fish
mkFish = One . (0,)

instance Semigroup Fish where
  a <> b = reduce (Two a b)

two :: GenParser Char () Fish
two = uncurry Two . toTuple2 <$> (char '[' *> (mkFish <$> number <|> two) `sepBy` char ',' <* char ']')

fish :: [Fish]
fish =
  snd $
    foldr
      (\f (nextIDs, fs) -> second (: fs) (setIDs f nextIDs))
      ([0 ..], [])
      (parseLinesWith two $(input 18))

setIDs :: Fish -> [FishID] -> ([FishID], Fish)
setIDs (One (_, a)) (nextID : nextIDs) = (nextIDs, One (nextID, a))
setIDs (Two a b) fishIDs =
  let (nextIDs', a') = setIDs a fishIDs
      (nextIDs'', b') = setIDs b nextIDs'
   in (nextIDs'', Two a' b')
setIDs f [] = ([], f)

fixExplode :: Fish -> Fish
fixExplode f =
  let f' = explodeFish f
   in if f == f' then f else fixExplode f'

reduce :: Fish -> Fish
reduce f =
  let f' = splitFish (fixExplode f)
   in if f == f' then f else reduce f'

splitFish :: Fish -> Fish
splitFish f' = case getSplitIds f' of
  [] -> f'
  (fishID : _) -> go fishID f'
  where
    nextID = maximum (fst <$> f') + 1
    go fishID f@(One (fishID', a))
      | fishID == fishID' =
        Two
          (One (nextID, floor (fromIntegral a / 2.0)))
          (One (nextID + 1, ceiling (fromIntegral a / 2.0)))
      | otherwise = f
    go fishID (Two a b) = Two (go fishID a) (go fishID b)
    getSplitIds (One (fishID, a))
      | a >= 10 = [fishID]
      | otherwise = []
    getSplitIds (Two a b) = getSplitIds a ++ getSplitIds b

ixMod :: (Int -> Int) -> FishID -> [FishID] -> Maybe FishID
ixMod ixF fishID fishIDs =
  case ixF <$> elemIndex fishID fishIDs of
    Nothing -> Nothing
    Just ix -> fishIDs !!? ix

runExplode :: FishID -> FishID -> Maybe FishID -> Maybe FishID -> Int -> Int -> Fish -> Fish
runExplode idA idB leftID rightID a b = go
  where
    go f@(One (fishID, x))
      | Just fishID == leftID = One (fishID, x + a)
      | Just fishID == rightID = One (fishID, x + b)
      | otherwise = f
    go (Two fa@(One (idA', _)) fb@(One (idB', _)))
      | idA == idA' && idB == idB' = One (idA', 0)
      | otherwise = Two (go fa) (go fb)
    go (Two a b) = Two (go a) (go b)

explodeFish :: Fish -> Fish
explodeFish f' =
  case go 0 f' of
    Nothing -> f'
    Just (idA, idB, a, b) ->
      let leftID = ixMod (subtract 1) idA fishIDs
          rightID = ixMod (+ 1) idB fishIDs
       in runExplode idA idB leftID rightID a b f'
  where
    fishIDs = foldr (\(id, _) ids -> id : ids) [] f'
    go _ (One _) = Nothing
    go 4 (Two (One (idA, a)) (One (idB, b))) = Just (idA, idB, a, b)
    go depth (Two a b) = go (depth + 1) a <|> go (depth + 1) b

magnitude :: Fish -> Int
magnitude (One (_, a)) = a
magnitude (Two a b) = 3 * magnitude a + 2 * magnitude b

part1 :: Int
part1 = magnitude $ foldl1' (<>) fish

part2 :: Int
part2 = L.maximum [magnitude (a <> b) | a <- fish, b <- fish, a /= b]
