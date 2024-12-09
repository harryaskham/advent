module Day9 (part1, part2) where

import Control.Applicative ((<|>))
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.RangeSet.Map qualified as RM
import Text.Show
import Prelude hiding (empty, filter, insert, insertRange, lookup, member, show, values, (<|>))

data Taggedℤ a = Taggedℤ {tagℤ :: a, valueℤ :: ℤ}

instance (Show a) => Show (Taggedℤ a) where
  show (Taggedℤ i n) = show i

instance Eq (Taggedℤ a) where
  (==) = (==) `on` valueℤ

instance Ord (Taggedℤ a) where
  compare = compare `on` valueℤ

instance Hashable (Taggedℤ a) where
  hashWithSalt s = hashWithSalt s ∘ valueℤ

instance (Num a, Eq a) => Enum (Taggedℤ a) where
  succ (Taggedℤ i n) = Taggedℤ i (succ n)
  pred (Taggedℤ i n) = Taggedℤ i (pred n)
  toEnum = Taggedℤ 0 ∘ fromIntegral
  fromEnum = fromIntegral . valueℤ
  enumFrom (Taggedℤ i x) = Taggedℤ i <$> enumFrom x
  enumFromThen (Taggedℤ i x) (Taggedℤ j y)
    | i ≡ j = Taggedℤ i <$> enumFromThen x y
    | otherwise = error "enumFromThen: different tags"
  enumFromTo (Taggedℤ i x) (Taggedℤ j y)
    | i ≡ j = Taggedℤ i <$> enumFromTo x y
    | otherwise = error "enumFromTo: different tags"
  enumFromThenTo (Taggedℤ i x) (Taggedℤ j y) (Taggedℤ k z)
    | i ≡ j && j ≡ k = Taggedℤ i <$> enumFromThenTo x y z
    | otherwise = error "enumFromThenTo: different tags"

class RangeMap m a | m -> a where
  empty :: m
  member :: a -> m -> Bool
  insert :: a -> m -> m
  default insert :: a -> m -> m
  insert a = insertRange (a, a)
  insertRange :: (a, a) -> m -> m
  isRangeEmpty :: (a, a) -> m -> Bool
  lookup :: a -> m -> a
  lookupM :: a -> m -> Maybe a
  filter :: (a -> Bool) -> m -> m
  values :: m -> [a]
  ranges :: m -> [(a, a)]

instance (Ord a, Enum a) => RangeMap (RM.RSet a) a where
  empty = RM.empty
  member = RM.member
  insertRange = RM.insertRange
  isRangeEmpty (a, b) m =
    let (_, m') = RM.split a m
        (m'', _) = RM.split b m'
     in RM.null m''
  lookup k m = unjust $ RM.lookupGE k m
  lookupM = RM.lookupGE
  filter = undefined
  values = undefined
  ranges = undefined

instance (Hashable a, Enum a) => RangeMap (HashMap a a) a where
  empty = HM.empty
  member = HM.member
  insertRange (a, b) m = foldl' (\m a -> HM.insert a a m) m (enumFromTo a b)
  isRangeEmpty (a, b) m = all isNothing (lookupM <$> enumFromTo a b <*> pure m)
  lookup k m = unjust $ HM.lookup k m
  lookupM = HM.lookup
  filter = HM.filter
  values = HM.keys
  ranges = undefined

data RangeMap' a = RangeMap' (Map (a, a) a) (Map a (a, a))

instance (Ord a) => RangeMap (RangeMap' a) a where
  empty = RangeMap' M.empty M.empty
  insertRange (a, b) m@(RangeMap' r2a a2r)
    | a `member` m || b `member` m = error "Only nonoverlapping supported"
    | otherwise =
        let rs = ranges m
         in case a2r |? a of
              Nothing -> RangeMap' (r2a |. ((a, a), a)) (a2r |. (a, (a, a)))
              Just (r0, r1) -> undefined
  isRangeEmpty (a, b) m = undefined
  lookup k m = undefined
  lookupM = undefined
  filter = undefined
  values = undefined

showRange :: (RangeMap m a, Enum a, Show a) => (a, a) -> m -> Text
showRange (a, b) m = mconcat [(tshow <$> lookupM i m) ? "." | i <- [a .. b]]

part1 :: ℤ
part1 =
  let freeOcc :: [(ℤ, ℤ)]
      freeOcc = ('0' : $(aoc 9)) |- many (twoOf (fromIntegral ∘ digitToInt <$> digit))
      (fillFrom, _) =
        foldl'
          (\(rm, address) (i, (free, occ)) -> (insertRange (Taggedℤ i address, Taggedℤ i (address + occ - 1)) rm, address + occ))
          (empty @(HashMap (Taggedℤ ℤ) (Taggedℤ ℤ)), 0)
          (reverse (zip [0 ..] freeOcc))
      (occRM, _, _) =
        foldl'
          ( \(rm, address, nFilled) (i, (free, occ)) ->
              let rm' =
                    foldl'
                      ( \rm offset ->
                          let Taggedℤ i _ = lookup (Taggedℤ 0 (nFilled + offset)) fillFrom
                           in insert (Taggedℤ i (address + offset)) rm
                      )
                      (insertRange (Taggedℤ i (address + free), Taggedℤ i (address + free + occ - 1)) rm)
                      [0 .. free - 1]
               in (rm', address + free + occ, nFilled + free)
          )
          (empty @(HashMap (Taggedℤ ℤ) (Taggedℤ ℤ)), 0, 0)
          (zip [0 ..] freeOcc)
      defragged [] = 0
      defragged (di : dis) =
        traceShow (length dis) $
          let Taggedℤ i addr = lookup (Taggedℤ 0 di) occRM
           in di ⋅ i + defragged dis
   in defragged [0 .. sum (snd <$> freeOcc) - 1]

part2 :: ℤ
part2 =
  let freeOcc :: [(ℤ, ℤ)]
      freeOcc = ('0' : $(aoc 9)) |- many (twoOf (fromIntegral ∘ digitToInt <$> digit))
      fileSizes = mkMap $ zip [0 ..] (snd <$> freeOcc)
      bound = sum ((+) <$@> freeOcc) - 1
      (occRM, _) =
        foldl'
          ( \(rm, address) (i, (free, occ)) ->
              let rm' = insertRange (Taggedℤ i (address + free), Taggedℤ i (address + free + occ - 1)) rm
               in (rm', address + free + occ)
          )
          (empty @(HashMap (Taggedℤ ℤ) (Taggedℤ ℤ)), 0)
          (zip [0 ..] freeOcc)
      filledRM =
        foldl'
          ( \rm (i, occ) ->
              traceShow i $
                let ibound = minimum (valueℤ <$> (values (filter ((≡ i) ∘ tagℤ) rm))) - 1
                    fits =
                      [ range
                        | start <- [0 .. ibound],
                          let range = (Taggedℤ i start, Taggedℤ i (start + occ - 1)),
                          isRangeEmpty range rm
                      ]
                 in case fits of
                      [] -> rm
                      (range : _) -> insertRange range (filter ((≢ i) ∘ tagℤ) rm)
          )
          occRM
          (reverse (zip [0 ..] (snd <$> freeOcc)))
      defragged [] _ = 0
      defragged (di : dis) seenBlocks =
        traceShow (length dis) $
          case lookupM (Taggedℤ 0 di) filledRM of
            Nothing -> defragged dis seenBlocks
            Just (Taggedℤ i addr) ->
              if (seenBlocks |! i) ≡ (fileSizes |! i)
                then 0
                else di ⋅ i + defragged dis (seenBlocks |~ (i, (+ 1)))
   in traceShow (showRange (Taggedℤ 0 0, Taggedℤ 0 bound) filledRM) $
        traceShow (showRange (Taggedℤ 0 0, Taggedℤ 0 bound) occRM) $
          defragged [0 .. bound] (const 0 <$> fileSizes)
