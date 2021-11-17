module TwentySixteen.Day11 where

import Data.List (sortOn)
import Data.List.Extra (breakOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    choice,
    eof,
    letter,
    many,
    many1,
    noneOf,
    sepBy,
    string,
    try,
  )
import Util (countMap, eol, input, readWithParser)

data Item
  = Microchip String
  | Generator String
  deriving (Show, Ord, Eq)

data FloorState = FloorState Int (Map Int (Set Item)) Int deriving (Show, Ord, Eq)

startState :: GenParser Char () FloorState
startState = do
  fs <- many floorItems
  eof
  return $ FloorState 1 (M.fromList fs) 0

floorItems :: GenParser Char () (Int, Set Item)
floorItems = do
  string "The "
  floorNum <- many1 letter
  string " floor contains "
  is <- choice [items, string "nothing relevant." >> return S.empty]
  eol
  return
    ( case floorNum of
        "first" -> 1
        "second" -> 2
        "third" -> 3
        "fourth" -> 4,
      is
    )

items :: GenParser Char () (Set Item)
items = do
  is <- item `sepBy` choice (try . string <$> [", and ", " and ", ", "])
  char '.'
  return (S.fromList is)

item :: GenParser Char () Item
item = do
  string "a "
  element <- many (noneOf " ")
  char ' '
  machine <- many1 letter
  return $ case machine of
    "generator" -> Generator element
    "microchip" -> Microchip (fst $ breakOn "-" element)

getMicrochip :: Item -> Maybe String
getMicrochip (Microchip n) = Just n
getMicrochip (Generator _) = Nothing

getGenerator :: Item -> Maybe String
getGenerator (Generator n) = Just n
getGenerator (Microchip _) = Nothing

getName :: Item -> String
getName (Generator n) = n
getName (Microchip n) = n

valid :: M.Map Int (Set Item) -> Bool
valid floorItems = all floorValid (M.elems floorItems)
  where
    mNames floor = S.fromList $ mapMaybe getMicrochip (S.toList floor)
    gNames floor = S.fromList $ mapMaybe getGenerator (S.toList floor)
    floorValid floor = mNames floor `S.isSubsetOf` gNames floor || onlyChips floor
    onlyChips floor = S.null (gNames floor)

terminal :: M.Map Int (Set Item) -> Bool
terminal floorItems = all S.null ((floorItems M.!) <$> [1, 2, 3])

type CacheKey = M.Map (Int, Int) Int

keyState :: Map Int (Set Item) -> CacheKey
keyState floorState = countMap $ zip generatorFloors microchipFloors
  where
    floorItems =
      sortOn
        (getName . snd)
        [ (f, i)
          | (f, is) <- M.toList floorState,
            i <- S.toList is
        ]
    generatorFloors = [f | (f, i) <- floorItems, isJust (getGenerator i)]
    microchipFloors = [f | (f, i) <- floorItems, isJust (getMicrochip i)]

bfs :: Seq FloorState -> Set (Int, CacheKey) -> Maybe Int
bfs SQ.Empty _ = Nothing
bfs (FloorState floor floorItems steps SQ.:<| rest) seen
  | terminal floorItems = Just steps
  | keyST `S.member` seen || not (valid floorItems) = bfs rest seen
  | otherwise = bfs (rest SQ.>< SQ.fromList nextStates) (S.insert keyST seen)
  where
    keyST = (floor, keyState floorItems)
    is = S.toList $ floorItems M.! floor
    toCarry = S.fromList <$> ((++) <$> (pure <$> is) <*> ([] : (pure <$> is)))
    nextFloors = [f | f <- [floor + 1, floor - 1], f >= 1, f <= 4]
    nextState f toCarry =
      FloorState
        f
        ( M.adjust (S.\\ toCarry) floor
            . M.adjust (S.union toCarry) f
            $ floorItems
        )
        (steps + 1)
    nextStates = nextState <$> nextFloors <*> toCarry

part1 :: IO (Maybe Int)
part1 = do
  start <- readWithParser startState <$> input 2016 11
  return $ bfs (SQ.singleton start) S.empty

part2 :: IO (Maybe Int)
part2 = do
  FloorState floor floorItems' step <-
    readWithParser startState <$> input 2016 11
  let floorItems =
        M.adjust
          ( S.union
              ( S.fromList
                  [ Generator "elerium",
                    Microchip "elerium",
                    Generator "dilithium",
                    Microchip "dilithium"
                  ]
              )
          )
          1
          floorItems'
  return $ bfs (SQ.singleton (FloorState floor floorItems step)) S.empty
