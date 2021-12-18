module Day18 where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Foldable (foldl1)
import Data.List (elemIndex, (!!))
import Data.List.Extra (foldl1')
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Unique
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util
import System.IO.Unsafe
import Text.ParserCombinators.Parsec hiding ((<|>))
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
mkFish a = One (nextFishId ()) a

instance Semigroup Fish where
  a <> b = traceShowF prettyFish $ reduce (Two a b)

prettyFish :: Fish -> Text
prettyFish (One _ a) = show a
prettyFish (Two a b) = "[" <> prettyFish a <> "," <> prettyFish b <> "]"

reduce :: Fish -> Fish
reduce f =
  traceTextLn (prettyFish f) $
    let f' = splitFish (explodeFish f)
     in if f == f' then f else reduce f'

splitFish :: Fish -> Fish
splitFish = go
  where
    go f@(One _ a)
      | a >= 10 =
        Two
          (mkFish (floor (fromIntegral a / 2.0)))
          (mkFish (ceiling (fromIntegral a / 2.0)))
      | otherwise = f
    go (Two a b) = Two (go a) (go b)

inorder :: Fish -> [UUID]
inorder (One uuid _) = [uuid]
inorder (Two a b) = inorder a ++ inorder b

addById :: Maybe UUID -> Int -> Fish -> Fish
addById uuidM x f' =
  case uuidM of
    Nothing -> f'
    Just uuid -> go uuid f'
  where
    go uuid f@(One uuid' a)
      | uuid == uuid' = One uuid' (a + x)
      | otherwise = f
    go uuid (Two a b) = Two (go uuid a) (go uuid b)

leftOf :: UUID -> [UUID] -> Maybe UUID
leftOf uuid uuids =
  let (Just ix) = subtract 1 <$> elemIndex uuid uuids
   in uuids !!? ix

rightOf :: UUID -> [UUID] -> Maybe UUID
rightOf uuid uuids =
  let (Just ix) = (+ 1) <$> elemIndex uuid uuids
   in uuids !!? ix

runExplode :: UUID -> UUID -> Maybe UUID -> Maybe UUID -> Int -> Int -> Fish -> Fish
runExplode idA idB leftID rightID a b =
  traceShow ("runnign explude", idA, idB, leftID, rightID, a, b) $
    go
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
        Nothing -> traceShow "nothing exploded" f'
        Just (idA, idB, a, b) ->
          traceShow ("something exploded:", idA, idB, a, b) $
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

--part1 :: Int
--part1 = magnitude $ foldl1' (<>) (parseLinesWith two $(input 18))
part1 = prettyFish $ foldl1' (<>) (parseLinesWith two $(exampleInputN 18 2))

--part1 = parseLinesWith two $(input 18)
--part1 = parseLinesWith two $(exampleInput 18)

part2 :: Text
part2 = "Part 2"
