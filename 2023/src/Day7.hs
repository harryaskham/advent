{-# LANGUAGE UndecidableInstances #-}

module Day7 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Tuple.Extra (uncurry3)
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util hiding (count)
import Text.ParserCombinators.Parsec

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA deriving (Eq, Ord, Show, Bounded, Enum)

newtype JCard = JCard Card deriving (Eq, Show)

instance Ord JCard where
  JCard CJ <= _ = True
  JCard a <= JCard b = a <= b

data HandType = High | Pair | TwoPair | Three | FullHouse | Four | Five deriving (Eq, Ord, Show, Bounded, Enum)

data Hand = Hand
  { _cards :: [Card],
    _bid :: Int
  }
  deriving (Eq, Show)

instance Ord Hand where
  (Hand cs _) <= (Hand cs' _) = (hand cs, cs) <= (hand cs', cs')

newtype JHand = JHand Hand deriving (Eq, Show)

instance Ord JHand where
  (JHand (Hand cs _)) <= (JHand (Hand cs' _)) = (jHand cs, JCard <$> cs) <= (jHand cs', JCard <$> cs')

-- -- instance (Ord b, HasCards a b, HasHand a) => Ord a where
-- instance Ord Hand where
--   h <= h' = (getHand h, getCards h) <= (getHand h', getCards h')

-- instance Ord JHand where
--   h <= h' = (getHand h, getCards h) <= (getHand h', getCards h')

class (Ord b) => HasCards a b where
  getCards :: a -> [b]

instance HasCards Hand Card where
  getCards Hand {_cards = cards} = cards

instance HasCards JHand JCard where
  getCards (JHand (Hand {_cards = cards})) = JCard <$> cards

class HasHand a where
  getHand :: a -> HandType

instance HasHand Hand where
  getHand Hand {_cards = cards} = hand cards

instance HasHand JHand where
  getHand (JHand (Hand {_cards = cards})) = jHand cards

class HasBid a where
  getBid :: a -> Int

instance HasBid Hand where
  getBid Hand {_bid = bid} = bid

instance HasBid JHand where
  getBid (JHand rh) = getBid rh

charToCard :: Char -> Card
charToCard '2' = C2
charToCard '3' = C3
charToCard '4' = C4
charToCard '5' = C5
charToCard '6' = C6
charToCard '7' = C7
charToCard '8' = C8
charToCard '9' = C9
charToCard 'T' = CT
charToCard 'J' = CJ
charToCard 'Q' = CQ
charToCard 'K' = CK
charToCard 'A' = CA

-- parser :: (Show a, Ord a, Ord b, HasBid a, HasHand a, HasCards a b) => (([Card], Int) -> a) -> Parser Int
parser :: (Show a, Ord a, HasBid a) => (([Card], Int) -> a) -> Parser Int
parser h = do
  cardsBids <- many1 ((,) <$> (charToCard <$$> (count 5 (oneOf "23456789TJKQA") <* string " ")) <*> (number <* eol)) <* eof
  let rankedHands = zip [1 ..] . sort $ h <$> cardsBids
  return . sum . fmap (\(r, b) -> r * getBid b) $ (fmap traceShowId) rankedHands

-- return . sum . fmap (\(r, b) -> r * getBid b) . zip [1 ..] . sort $ (\(cards, bid) -> h (hand cards, cards, bid)) <$> cardsBids

hand :: [Card] -> HandType
hand cards
  | isJust $ M.lookup 5 reverseCounts = Five
  | isJust $ M.lookup 4 reverseCounts = Four
  | isJust (M.lookup 3 reverseCounts) && isJust (M.lookup 2 reverseCounts) =
      FullHouse
  | isJust $ M.lookup 3 reverseCounts = Three
  | isJust $ M.lookup 2 reverseCounts =
      case reverseCounts M.! 2 of
        [_] -> Pair
        [_, _] -> TwoPair
  | otherwise = High
  where
    reverseCounts = swapMapCollect $ countMap cards

jHand :: [Card] -> HandType
jHand cards
  | isJust $ M.lookup 5 reverseCounts = Five
  | isJust $ M.lookup 4 reverseCounts =
      case nj of
        4 -> Five
        1 -> Five
        _ -> Four
  | isJust (M.lookup 3 reverseCounts) && isJust (M.lookup 2 reverseCounts) =
      case nj of
        3 -> Five
        2 -> Five
        _ -> FullHouse
  | isJust $ M.lookup 3 reverseCounts =
      -- the jokers can become different cards to one another
      case nj of
        3 -> Four
        2 -> Five
        1 -> Four
        _ -> Three
  | isJust $ M.lookup 2 reverseCounts =
      case reverseCounts M.! 2 of
        [_] -> case nj of
          2 -> Three
          1 -> Three
          _ -> Pair
        [_, _] -> case nj of
          2 -> Four
          1 -> FullHouse
          _ -> TwoPair
  | otherwise =
      case nj of
        1 -> Pair
        _ -> High
  where
    counts = countMap cards
    reverseCounts = swapMapCollect counts
    nj = fromMaybe 0 $ M.lookup CJ counts

part1 :: Int
part1 = $(input 7) & parseWith (parser (uncurry Hand))

-- too high 250286820
-- too high 250357779
part2 :: Int
part2 = $(input 7) & parseWith (parser (JHand . uncurry Hand))

-- part2 = $(exampleInput 7) & parseWith (parser (JHand . uncurry Hand))