module Helper.Util where

import Control.Arrow (Arrow ((***)))
import Control.Monad (filterM)
import Data.List ((!!))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Tuple.Extra (swap)
import Relude.Unsafe (read)
import Text.ParserCombinators.Parsec (GenParser, char, eof, many1, oneOf, parse, sepBy)

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

-- Typeclass helpers

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

same :: Eq a => (a, a) -> Bool
same = uncurry (==)

-- Specific currying / conversions

toTuple2 :: [a] -> (a, a)
toTuple2 [a, b] = (a, b)
toTuple2 xs = error $ show (length xs) <> "elements in toTuple2"

toTuple3 :: [a] -> Maybe (a, a, a)
toTuple3 [a, b, c] = Just (a, b, c)
toTuple3 _ = Nothing

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

first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (a, b, c) = (f a, b, c)

-- Parser Combinator Utils

eol :: GenParser Char () Char
eol = char '\n'

number :: Read a => GenParser Char () a
number = read <$> many1 (oneOf "-0123456789")

bitChar :: GenParser Char () Bool
bitChar = (char '1' >> return True) <|> (char '0' >> return False)

coord2 :: GenParser Char () (Int, Int)
coord2 = (,) <$> (number <* char ',') <*> number

csvLine :: GenParser Char () a -> GenParser Char () [a]
csvLine a = a `sepBy` char ',' <* (eol >> eof)

-- Map helpers

countMap :: Ord a => [a] -> M.Map a Int
countMap xs = getSum <$> M.fromListWith (+) (zip xs (repeat $ Sum 1))

adjustWithDefault :: Ord k => a -> (a -> a) -> k -> M.Map k a -> M.Map k a
adjustWithDefault def f k m = case M.lookup k m of
  Nothing -> M.insert k (f def) m
  Just a -> M.insert k (f a) m

swapMap :: Ord b => M.Map a b -> M.Map b a
swapMap = M.fromList . fmap swap . M.toList

-- List helpers

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

batch3 :: [a] -> [[a]]
batch3 xs = toList3 <$> zip3 (drop 2 xs) (drop 1 xs) xs

pair :: a -> a -> [a]
pair a b = [a, b]

about :: Integral a => a -> a -> [a]
about n x = [x - n .. x + n]

-- How many xs match predicate p
count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

-- Returns unique members of a list that appear more than once
duplicates :: Ord a => [a] -> [a]
duplicates xs = M.keys $ M.filter (> 1) (countMap xs)

-- A Solution typeclass for objects that end up representing the solution in some way

class Solution a b where
  toSolution :: a -> b

-- Arrow helpers

-- A symmetrical split
split :: Arrow a => a b c -> a (b, b) (c, c)
split f = f *** f

-- A symmetrical fanout
fanout :: Arrow a => a b c -> a b (c, c)
fanout f = f &&& f

-- Mathy utils

-- nth triangular number
triangular :: Integral a => a -> a
triangular n = n * (n + 1) `div` 2

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

mean :: Integral a => [a] -> a
mean xs = sum xs `div` fromIntegral (length xs)
