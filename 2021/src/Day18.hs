module Day18 where

import Data.Foldable (foldl1)
import Data.List (elemIndex, (!!))
import Data.List qualified as L
import Data.List.Extra (foldl1')
import Helper.TH (input)
import Helper.Util (number, parseLinesWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, char, sepBy)

type FishID = Integer

data Fish
  = One FishID Int
  | Two Fish Fish
  deriving (Eq, Ord, Show)

instance Semigroup Fish where
  a <> b = reduce (Two a b)

setIDs :: [FishID] -> Fish -> (Fish, [FishID])
setIDs (nextID : nextIDs) (One _ a) = (One nextID a, nextIDs)
setIDs nextIDs (Two a b) =
  let (a', nextIDs') = setIDs nextIDs a
      (b', nextIDs'') = setIDs nextIDs' b
   in (Two a' b', nextIDs'')
setIDs [] _ = error "Not enough IDs provided"

prettyFish :: Fish -> Text
prettyFish (One _ a) = show a
prettyFish (Two a b) = "[" <> prettyFish a <> "," <> prettyFish b <> "]"

fixExplode :: Fish -> Fish
fixExplode f =
  let f' = explodeFish f
   in if f == f' then f else fixExplode f'

fixSplit :: Fish -> Fish
fixSplit f =
  let f' = splitFish f
   in if f == f' then f else fixSplit f'

reduce :: Fish -> Fish
reduce f =
  let f' = splitFish (fixExplode f)
   in if f == f' then f else reduce f'

getSplitIds :: Fish -> [FishID]
getSplitIds = go
  where
    go (One fishID a)
      | a >= 10 = [fishID]
      | otherwise = []
    go (Two a b) = getSplitIds a ++ getSplitIds b

maxID :: Fish -> FishID
maxID (One fishID _) = fishID
maxID (Two a b) = max (maxID a) (maxID b)

splitFish :: Fish -> Fish
splitFish f' = case getSplitIds f' of
  [] -> f'
  (fishID : _) -> go fishID f'
  where
    id1 = maxID f' + 1
    id2 = id1 + 1
    go fishID f@(One fishID' a)
      | fishID == fishID' =
        Two
          (One id1 (floor (fromIntegral a / 2.0)))
          (One id2 (ceiling (fromIntegral a / 2.0)))
      | otherwise = f
    go fishID (Two a b) = Two (go fishID a) (go fishID b)

inorder :: Fish -> [FishID]
inorder (One fishID _) = [fishID]
inorder (Two a b) = inorder a ++ inorder b

leftOf :: FishID -> [FishID] -> Maybe FishID
leftOf fishID fishIDs =
  let (Just ix) = subtract 1 <$> elemIndex fishID fishIDs
   in fishIDs !!? ix

rightOf :: FishID -> [FishID] -> Maybe FishID
rightOf fishID fishIDs =
  let (Just ix) = (+ 1) <$> elemIndex fishID fishIDs
   in fishIDs !!? ix

runExplode :: FishID -> FishID -> Maybe FishID -> Maybe FishID -> Int -> Int -> Fish -> Fish
runExplode idA idB leftID rightID a b f' = go f'
  where
    nextID = maxID f' + 1
    go f@(One fishID x)
      | Just fishID == leftID = One fishID (x + a)
      | Just fishID == rightID = One fishID (x + b)
      | otherwise = f
    go (Two fa@(One idA' _) fb@(One idB' _))
      | idA == idA' && idB == idB' = One nextID 0
      | otherwise = Two (go fa) (go fb)
    go (Two a b) = Two (go a) (go b)

explodeFish :: Fish -> Fish
explodeFish f' =
  let e = go 0 f'
      fishIDs = inorder f'
   in case e of
        Nothing -> f'
        Just (idA, idB, a, b) ->
          let leftID = leftOf idA fishIDs
              rightID = rightOf idB fishIDs
           in runExplode idA idB leftID rightID a b f'
  where
    go :: Int -> Fish -> Maybe (FishID, FishID, Int, Int)
    go _ (One _ _) = Nothing
    go 4 (Two (One idA a) (One idB b)) = Just (idA, idB, a, b)
    go depth (Two a b) =
      let e1 = go (depth + 1) a
          e2 = go (depth + 1) b
       in case e1 of
            Just _ -> e1
            Nothing -> case e2 of
              Just _ -> e2
              Nothing -> Nothing

magnitude :: Fish -> Int
magnitude (One _ a) = a
magnitude (Two a b) = 3 * magnitude a + 2 * magnitude b

two :: GenParser Char () Fish
two = uncurry Two . toTuple2 <$> (char '[' *> (One 0 <$> number <|> two) `sepBy` char ',' <* char ']')

fish :: [Fish]
fish =
  snd $
    foldr
      (\f (nextIDs, fs) -> let (f', nextIDs') = setIDs nextIDs f in (nextIDs', f' : fs))
      ([0 ..], [])
      (parseLinesWith two $(input 18))

part1 :: Int
part1 = magnitude $ foldl1' (<>) fish

part2 :: Int
part2 = L.maximum [magnitude (a <> b) | a <- fish, b <- fish, a /= b]
