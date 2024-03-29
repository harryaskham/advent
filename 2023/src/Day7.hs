module Day7 (part1, part2) where

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA deriving (Eq, Ord, Bounded, Enum)

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

newtype JCard = JCard Card deriving (Eq)

instance Ord JCard where
  JCard CJ <= JCard CJ = True
  JCard CJ <= _ = True
  JCard _ <= JCard CJ = False
  JCard a <= JCard b = a <= b

data Hand = High | Pair | TwoPair | Three | FullHouse | Four | Five deriving (Eq, Ord, Bounded, Enum)

data HandBid = HandBid [Card] ℤ' deriving (Eq)

instance Ord HandBid where
  h@(HandBid cs _) <= h'@(HandBid cs' _) = (getHand h, cs) <= (getHand h', cs')

newtype JHand = JHand HandBid deriving (Eq)

instance Ord JHand where
  h@(JHand (HandBid cs _)) <= h'@(JHand (HandBid cs' _)) = (getHand h, JCard <$> cs) <= (getHand h', JCard <$> cs')

class HasHand a where
  getHand :: a -> Hand

instance HasHand HandBid where
  getHand (HandBid cards _)
    | isJust $ reverseCounts |? 5 = Five
    | isJust $ reverseCounts |? 4 = Four
    | isJust (reverseCounts |? 3) && isJust (reverseCounts |? 2) = FullHouse
    | isJust $ reverseCounts |? 3 = Three
    | isJust $ reverseCounts |? 2 =
        case reverseCounts |! 2 of
          [_] -> Pair
          [_, _] -> TwoPair
    | otherwise = High
    where
      reverseCounts = swapMapCollect $ countMap cards

instance HasHand JHand where
  getHand (JHand (HandBid cards _))
    | isJust $ reverseCounts |? 5 = Five
    | isJust $ reverseCounts |? 4 =
        case nj of
          4 -> Five
          1 -> Five
          _ -> Four
    | isJust (reverseCounts |? 3) && isJust (reverseCounts |? 2) =
        case nj of
          3 -> Five
          2 -> Five
          _ -> FullHouse
    | isJust $ reverseCounts |? 3 =
        case nj of
          3 -> Four
          1 -> Four
          _ -> Three
    | isJust $ reverseCounts |? 2 =
        case reverseCounts |! 2 of
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
      nj = fromMaybe 0 $ counts |? CJ

class HasBid a where
  getBid :: a -> ℤ'

instance HasBid HandBid where
  getBid (HandBid _ bid) = bid

instance HasBid JHand where
  getBid (JHand rh) = getBid rh

parser :: (Ord a, HasBid a) => (([Card], ℤ') -> a) -> Parser ℤ'
parser h = do
  cardsBids <- many1 ((,) <$> (charToCard <$$> (count 5 (oneOf "23456789TJKQA") <* string " ")) <*> (number <* eol)) <* eof
  return . sum . fmap (\(r, b) -> r * getBid b) $ zip [1 ..] . sort $ h <$> cardsBids

part1 :: ℤ'
part1 = $(input 7) ⊢ parser (uncurry HandBid)

part2 :: ℤ'
part2 = $(input 7) ⊢ parser (JHand . uncurry HandBid)