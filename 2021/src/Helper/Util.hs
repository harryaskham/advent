module Helper.Util where

import Control.Arrow (Arrow ((***)))
import Control.Lens ((^.))
import Control.Monad (filterM)
import Data.List ((!!))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Ord (Down (Down))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Tuple.Extra (swap)
import Data.Tuple.HT (uncurry3)
import Data.Type.Nat (Nat (S), Nat9)
import Helper.Bits (bitsToInt)
import Linear.V3 (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import Relude.Unsafe (read)
import Text.ParserCombinators.Parsec (GenParser, char, count, eof, many1, oneOf, parse, sepBy)

-- Input parsing

inputPath :: Int -> FilePath
inputPath day = "input/" <> show day <> ".txt"

exampleInputPath :: Int -> FilePath
exampleInputPath day = "input/" <> show day <> "_example.txt"

exampleInputNPath :: Int -> Int -> FilePath
exampleInputNPath day n = "input/" <> show day <> "_example_" <> show n <> ".txt"

-- Parse input lines with the given Reader.
-- If any error occurs, fail.
readAsIO :: TR.Reader a -> FilePath -> IO [a]
readAsIO r path = readAs r <$> readFileText path

readAs :: TR.Reader a -> Text -> [a]
readAs r text = do
  let xs = fmap r . lines $ text
  let (ls, rs) = partitionEithers xs
  case ls of
    [] -> fst <$> rs
    es -> error (show es)

readOne :: TR.Reader a -> Text -> a
readOne r text =
  case r text of
    Left e -> error (show e)
    Right (a, _) -> a

-- Helper utility for running a parser on a Text path
parseWithIO :: GenParser Char () a -> FilePath -> IO a
parseWithIO parser path = parseWith parser <$> readFile (toString path)

parseWith :: GenParser Char () a -> String -> a
parseWith parser body =
  case parse parser "[input]" body of
    Right x -> x
    Left e -> error (show e)

parseLinesWith :: GenParser Char () a -> String -> [a]
parseLinesWith line = parseWith $ many1 (line <* eol) <* eof

-- Typeclass helpers / functional helpers

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

same :: Eq a => (a, a) -> Bool
same = uncurry (==)

iterateFix :: Eq a => (a -> a) -> a -> a
iterateFix f a
  | a == a' = a
  | otherwise = iterateFix f a'
  where
    a' = f a

-- A symmetrical split
split :: Arrow a => a b c -> a (b, b) (c, c)
split f = f *** f

-- A symmetrical fanout
fanout :: Arrow a => a b c -> a b (c, c)
fanout f = f &&& f

-- Apply the given function only if the predicate holds on the input
appWhen :: (a -> Bool) -> (a -> a) -> a -> a
appWhen p f x
  | p x = f x
  | otherwise = x

-- Specific currying / conversions

toTuple2 :: [a] -> (a, a)
toTuple2 [a, b] = (a, b)
toTuple2 xs = error $ show (length xs) <> "elements in toTuple2"

toTuple3 :: [a] -> (a, a, a)
toTuple3 [a, b, c] = (a, b, c)
toTuple3 xs = error $ show (length xs) <> "elements in toTuple3"

toTuple4 :: [a] -> (a, a, a, a)
toTuple4 [a, b, c, d] = (a, b, c, d)
toTuple4 xs = error $ show (length xs) <> "elements in toTuple4"

toList3 :: (a, a, a) -> [a]
toList3 (a, b, c) = [a, b, c]

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f = let f' (a, b, c, d) = f a b c d in f'

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, a, _, _) = a

thd4 :: (a, b, c, d) -> c
thd4 (_, _, a, _) = a

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, a) = a

perms3 :: [(Int, Int, Int) -> (Int, Int, Int)]
perms3 =
  [ \(x, y, z) -> (x, y, z),
    \(x, y, z) -> (x, z, y),
    \(x, y, z) -> (y, x, z),
    \(x, y, z) -> (y, z, x),
    \(x, y, z) -> (z, x, y),
    \(x, y, z) -> (z, y, x)
  ]

permsV3 :: [V3 a -> V3 a]
permsV3 =
  [ id,
    \p -> uncurry3 V3 (p ^. _x, p ^. _z, p ^. _y),
    \p -> uncurry3 V3 (p ^. _y, p ^. _x, p ^. _z),
    \p -> uncurry3 V3 (p ^. _y, p ^. _z, p ^. _x),
    \p -> uncurry3 V3 (p ^. _z, p ^. _x, p ^. _y),
    \p -> uncurry3 V3 (p ^. _z, p ^. _y, p ^. _x)
  ]

fromV3 :: V3 a -> (a, a, a)
fromV3 p = (p ^. _x, p ^. _y, p ^. _z)

toV3 :: (a, a, a) -> V3 a
toV3 = uncurry3 V3

-- Parser Combinator Utils

eol :: GenParser Char () Char
eol = char '\n'

number :: Read a => GenParser Char () a
number = read <$> many1 (oneOf "-0123456789")

bitChar :: GenParser Char () Bool
bitChar = (char '1' >> return True) <|> (char '0' >> return False)

nBitInt :: Int -> GenParser Char () Integer
nBitInt n = bitsToInt <$> Text.ParserCombinators.Parsec.count n bitChar

coord2 :: GenParser Char () (Int, Int)
coord2 = (,) <$> (number <* char ',') <*> number

csvLine :: GenParser Char () a -> GenParser Char () [a]
csvLine a = a `sepBy` char ',' <* (eol >> eof)

-- Map helpers

countMap :: Ord a => [a] -> M.Map a Int
countMap xs = M.fromListWith (+) (zip xs (repeat 1))

adjustWithDefault :: Ord k => a -> (a -> a) -> k -> M.Map k a -> M.Map k a
adjustWithDefault def f k m = case M.lookup k m of
  Nothing -> M.insert k (f def) m
  Just a -> M.insert k (f a) m

adjustMany :: Ord k => (a -> a) -> [k] -> M.Map k a -> M.Map k a
adjustMany f ks m = foldl' (flip (M.adjust f)) m ks

swapMap :: Ord b => M.Map a b -> M.Map b a
swapMap = M.fromList . fmap swap . M.toList

-- Set helpers

insertMany :: Ord a => [a] -> Set a -> Set a
insertMany as s = foldl' (flip S.insert) s as

-- List helpers

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

batch3 :: [a] -> [[a]]
batch3 xs = toList3 <$> zip3 (drop 2 xs) (drop 1 xs) xs

pair :: a -> a -> [a]
pair a b = [a, b]

about :: Integral a => a -> a -> [a]
about n x = [x - n .. x + n]

listAsInt :: Integral a => [a] -> a
listAsInt xs = sum $ uncurry (*) <$> zip (reverse xs) [10 ^ i | i <- [0 :: Integer ..]]

unlist :: [a] -> a
unlist [a] = a
unlist as = error ("Unlist called on list of length " <> show (length as))

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (a : b : cs) = (a, b) : pairs (b : cs)

-- Early terminating search for n items in a thing
nSameIn :: Ord a => Int -> [a] -> Maybe a
nSameIn n = go M.empty
  where
    go _ [] = Nothing
    go counts (a : as)
      | counts' M.! a == n = Just a
      | otherwise = go counts' as
      where
        counts' = adjustWithDefault 0 (+ 1) a counts

-- How many xs match predicate p
count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

-- Returns unique members of a list that appear more than once
duplicates :: Ord a => [a] -> [a]
duplicates xs = M.keys $ M.filter (> 1) (countMap xs)

enumerate :: Enum a => [a]
enumerate = enumFrom (toEnum 0)

permutationMaps :: (Enum a, Ord a) => [Map a a]
permutationMaps = M.fromList . zip enumerate <$> permutations enumerate

permuteSet :: Ord a => Map a a -> Set a -> Set a
permuteSet = S.map . (M.!)

-- A Solution typeclass for objects that end up representing the solution in some way

class Solution a b where
  toSolution :: a -> b

-- Mathy utils

-- nth triangular number
triangular :: Integral a => a -> a
triangular n = n * (n + 1) `div` 2

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

mean :: Integral a => [a] -> a
mean xs = sum xs `div` fromIntegral (length xs)

type Nat10 = 'S Nat9
