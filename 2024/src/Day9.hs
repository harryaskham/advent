module Day9 (part1, part2) where

import Control.Applicative ((<|>))
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.RangeSet.Map qualified as RM
import Data.Set qualified as S
import Text.Show
import Prelude hiding (empty, insert, insertRange, lookup, member, show, values, (<|>))

data Taggedℤ a = Taggedℤ {tagℤ :: a, valueℤ :: ℤ}

type family CarryF a where
  CarryF (Taggedℤ a) = a

class Carry a where
  carry :: a -> CarryF a

instance Carry (Taggedℤ a) where
  carry = tagℤ

instance (Show a) => Show (Taggedℤ a) where
  show (Taggedℤ i n) = show i

instance Eq (Taggedℤ a) where
  (==) = (==) `on` valueℤ

instance Ord (Taggedℤ a) where
  compare = compare `on` valueℤ

instance Hashable (Taggedℤ a) where
  hashWithSalt s = hashWithSalt s ∘ valueℤ

instance (Num a) => Num (Taggedℤ a) where
  Taggedℤ i a + Taggedℤ _ b = Taggedℤ i (a + b)
  Taggedℤ i a - Taggedℤ _ b = Taggedℤ i (a - b)
  Taggedℤ i a * Taggedℤ _ b = Taggedℤ i (a * b)
  negate (Taggedℤ i a) = Taggedℤ i (negate a)
  abs (Taggedℤ i a) = Taggedℤ i (abs a)
  signum (Taggedℤ i a) = Taggedℤ i (signum a)
  fromInteger n = Taggedℤ 0 n

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
  ranges :: m -> [(a, a)]
  rangeFor :: (Carry a) => (CarryF a) -> m -> (a, a)
  without :: (Carry a) => (CarryF a) -> m -> m
  slot :: a -> (a, a) -> m -> Maybe (a, a)

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
  ranges = undefined
  rangeFor = undefined
  without = undefined
  slot = undefined

instance (Hashable a, Enum a) => RangeMap (HashMap a a) a where
  empty = HM.empty
  member = HM.member
  insertRange (a, b) m = foldl' (\m a -> HM.insert a a m) m (enumFromTo a b)
  isRangeEmpty (a, b) m = all isNothing (lookupM <$> enumFromTo a b <*> pure m)
  lookup k m = unjust $ HM.lookup k m
  lookupM = HM.lookup
  ranges = undefined
  rangeFor = undefined
  without = undefined
  slot = undefined

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
  rangeFor = undefined
  without = undefined
  slot = undefined

data RangeSet a = RangeSet (Set (a, a)) (Map (CarryF a) (a, a))

overlap :: (Ord a) => (a, a) -> (a, a) -> Bool
overlap (a, b) (c, d) = b ≥ c ∧ a ≤ d

contains :: (Ord a) => a -> (a, a) -> Bool
contains a (b, c) = overlap (a, a) (b, c)

inside :: (Ord a) => (a, a) -> (a, a) -> Bool
inside (a, b) c = contains a c ∧ contains b c

instance (Ord a, Enum a, Num a, Carry a, Ord (CarryF a)) => RangeMap (RangeSet a) a where
  empty = RangeSet S.empty M.empty
  member a rs@(RangeSet s m) = carry a ∈ m
  insertRange (a, b) (RangeSet s m) = RangeSet ((a, b) |-> s) (m |. (carry a, (a, b)))
  isRangeEmpty a m = all (≡ False) [overlap a r | r <- ranges m]
  lookup a m = unjust $ lookupM a m
  lookupM a m = case [r | r <- ranges m, contains a r] of
    [] -> Nothing
    ((r0, _) : _) -> Just r0
  ranges (RangeSet s m) = unSet s
  rangeFor a (RangeSet s m) = m |! a
  without i (RangeSet s m) = let r = m |! i in RangeSet (s |\ r) (m |\ i)
  slot n bounds@(r0, r1) m
    | r1 - r0 + 1 < n = Nothing
    | otherwise =
        case sort (ranges m) of
          [] -> Just (r0, r0 + n - 1)
          [(a, b)] -> if a >= n then Just (0, n - 1) else Just (b + 1, b + n)
          rs@((a0, b) : _) ->
            let gaps = (0, a0 - 1) : [(b + 1, c - 1) | ((a, b), (c, d)) <- zip rs (drop 1 rs)]
                fits = nonEmpty [(a, a + n - 1) | gap@(a, b) <- gaps, gap `inside` bounds, let placed = (a, a + n - 1), placed `inside` gap]
             in head <$> fits

showRange :: (RangeMap m a, Enum a, Show a) => (a, a) -> m -> Text
showRange (a, b) m = mconcat [(tshow <$> lookupM i m) ? "." | i <- [a .. b]]

part1 :: ℤ
part1 =
  let freeOcc :: [(ℤ, ℤ)]
      freeOcc = ('0' : $(aocx 9)) |- many (twoOf (fromIntegral ∘ digitToInt <$> digit))
      (fillFrom, _) =
        foldl'
          (\(rm, address) (i, (free, occ)) -> (insertRange (Taggedℤ i address, Taggedℤ i (address + occ - 1)) rm, address + occ))
          (empty @(RangeSet (Taggedℤ ℤ)), 0)
          -- (empty @(HashMap (Taggedℤ ℤ) (Taggedℤ ℤ)), 0)
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
               in traceShow (showRange (Taggedℤ 0 0, Taggedℤ 0 50) rm') $ (rm', address + free + occ, nFilled + free)
          )
          -- (empty @(HashMap (Taggedℤ ℤ) (Taggedℤ ℤ)), 0, 0)
          (empty @(RangeSet (Taggedℤ ℤ)), 0, 0)
          (zip [0 ..] freeOcc)
      defragged [] = 0
      defragged (di : dis) =
        let Taggedℤ i addr = lookup (Taggedℤ 0 di) occRM
         in di ⋅ i + defragged dis
   in defragged [0 .. sum (snd <$> freeOcc) - 1]

part2 :: ℤ
part2 =
  let freeOcc :: [(ℤ, ℤ)]
      freeOcc = ('0' : $(aoc 9)) |- many (twoOf (fromIntegral ∘ digitToInt <$> digit))
      fileSizes = mkMap $ zip [0 ..] (snd <$> freeOcc)
      bound = sum ((+) <$@> freeOcc) - 1
      scoreRange (Taggedℤ i a, Taggedℤ _ b) = i ⋅ (triangular b - triangular (a - 1))
      (occRM, _, occScore) =
        foldl'
          ( \(rm, address, rmScore) (i, (free, occ)) ->
              let r = (Taggedℤ i (address + free), Taggedℤ i (address + free + occ - 1))
               in (insertRange r rm, address + free + occ, rmScore + scoreRange r)
          )
          (empty @(RangeSet (Taggedℤ ℤ)), 0, 0)
          (zip [0 ..] freeOcc)
      (filledRM, filledScore) =
        foldl'
          ( \(rm, rmScore) (i, occ) ->
              traceShow (i, rmScore) $
                let ir@(ilb, _) = rangeFor i rm
                 in case slot (Taggedℤ 0 occ) (0, pred ilb) rm of
                      Nothing -> (rm, rmScore)
                      Just (Taggedℤ _ a, Taggedℤ _ b) ->
                        let r = (Taggedℤ i a, Taggedℤ i b)
                         in (insertRange r . without i $ rm, rmScore + scoreRange r - scoreRange ir)
          )
          (occRM, occScore)
          (reverse (zip [0 ..] (snd <$> freeOcc)))
   in filledScore
