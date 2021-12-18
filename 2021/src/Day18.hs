module Day18 where

import Data.Foldable (foldl1)
import Data.List (elemIndex, (!!))
import Data.List qualified as L
import Data.List.Extra (foldl1')
import Data.Unique (Unique, hashUnique, newUnique)
import Helper.TH (input)
import Helper.Util (number, parseLinesWith, toTuple2)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec (GenParser, char, sepBy)
import Text.Show (showsPrec)

newtype UUID = UUID Unique deriving (Eq, Ord)

instance Show UUID where
  showsPrec p (UUID a) = showsPrec p (hashUnique a)

data Fish
  = One UUID Int
  | Two Fish Fish
  deriving (Eq, Ord, Show)

nextFishId :: () -> UUID
{-# NOINLINE nextFishId #-}
nextFishId () = UUID $ unsafePerformIO newUnique

mkFish :: Int -> Fish
mkFish = One (nextFishId ())

instance Semigroup Fish where
  a <> b = reduce (Two a b)

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

getSplitIds :: Fish -> [UUID]
getSplitIds = go
  where
    go (One uuid a)
      | a >= 10 = [uuid]
      | otherwise = []
    go (Two a b) = getSplitIds a ++ getSplitIds b

splitFish :: Fish -> Fish
splitFish f' = case getSplitIds f' of
  [] -> f'
  (uuid : _) -> go uuid f'
  where
    go uuid f@(One uuid' a)
      | uuid == uuid' =
        Two
          (mkFish (floor (fromIntegral a / 2.0)))
          (mkFish (ceiling (fromIntegral a / 2.0)))
      | otherwise = f
    go uuid (Two a b) = Two (go uuid a) (go uuid b)

inorder :: Fish -> [UUID]
inorder (One uuid _) = [uuid]
inorder (Two a b) = inorder a ++ inorder b

leftOf :: UUID -> [UUID] -> Maybe UUID
leftOf uuid uuids =
  let (Just ix) = subtract 1 <$> elemIndex uuid uuids
   in uuids !!? ix

rightOf :: UUID -> [UUID] -> Maybe UUID
rightOf uuid uuids =
  let (Just ix) = (+ 1) <$> elemIndex uuid uuids
   in uuids !!? ix

runExplode :: UUID -> UUID -> Maybe UUID -> Maybe UUID -> Int -> Int -> Fish -> Fish
runExplode idA idB leftID rightID a b = go
  where
    go f@(One uuid x)
      | Just uuid == leftID = One uuid (x + a)
      | Just uuid == rightID = One uuid (x + b)
      | otherwise = f
    go (Two fa@(One idA' _) fb@(One idB' _))
      | idA == idA' && idB == idB' = mkFish 0
      | otherwise = Two (go fa) (go fb)
    go (Two a b) = Two (go a) (go b)

explodeFish :: Fish -> Fish
explodeFish f' =
  let e = go 0 f'
      uuids = inorder f'
   in case e of
        Nothing -> f'
        Just (idA, idB, a, b) ->
          let leftID = leftOf idA uuids
              rightID = rightOf idB uuids
           in runExplode idA idB leftID rightID a b f'
  where
    go :: Int -> Fish -> Maybe (UUID, UUID, Int, Int)
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
two = uncurry Two . toTuple2 <$> (char '[' *> (mkFish <$> number <|> two) `sepBy` char ',' <* char ']')

part1 :: Int
part1 = magnitude $ foldl1' (<>) (parseLinesWith two $(input 18))

part2 :: Int
part2 =
  let xs = parseLinesWith two $(input 18)
   in L.maximum [magnitude (a <> b) | a <- xs, b <- xs, a /= b]
