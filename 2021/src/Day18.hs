module Day18 (part1, part2) where

import Data.Foldable (maximum)
import Data.List (elemIndex)
import Data.List.Extra (foldl1')
import Helper.TH (input)
import Helper.Util (number, parseLinesWith, toTuple2)
import Safe (headMay)
import Text.ParserCombinators.Parsec (GenParser, char, sepBy)

type FishID = Integer

data Fish' a
  = One a
  | Two (Fish' a) (Fish' a)
  deriving (Eq, Ord, Show, Functor, Foldable)

type Fish = Fish' (FishID, Int)

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

reduce :: Fish -> Fish
reduce f
  | f /= f' = reduce f'
  | f /= f'' = reduce f''
  | otherwise = f
  where
    f' = explodeFish f
    f'' = splitFish f

mapID :: (Fish -> Fish) -> FishID -> Fish -> Fish
mapID f fishID = go
  where
    go :: Fish -> Fish
    go fish@(One (fishID', _))
      | fishID == fishID' = f fish
      | otherwise = fish
    go (Two a b) = Two (go a) (go b)

splitFish :: Fish -> Fish
splitFish f' =
  case headMay [fishID | (fishID, a) <- toList f', a >= 10] of
    Nothing -> f'
    Just fishID -> mapID splitOne fishID f'
  where
    nextID = maximum (fst <$> f') + 1
    splitOne (One (_, a)) =
      Two
        (One (nextID, floor (fromIntegral a / 2.0)))
        (One (nextID + 1, ceiling (fromIntegral a / 2.0)))
    splitOne fish@(Two _ _) = fish

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
    fishIDs = toList (fst <$> f')
    go _ (One _) = Nothing
    go 4 (Two (One (idA, a)) (One (idB, b))) = Just (idA, idB, a, b)
    go depth (Two a b) = go (depth + 1) a <|> go (depth + 1) b

magnitude :: Fish -> Int
magnitude (One (_, a)) = a
magnitude (Two a b) = 3 * magnitude a + 2 * magnitude b

part1 :: Int
part1 = magnitude $ foldl1' (<>) fish

part2 :: Int
part2 = maximum [magnitude (a <> b) | a <- fish, b <- fish, a /= b]
