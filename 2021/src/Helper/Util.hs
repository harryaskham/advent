module Helper.Util where

import Control.Monad (filterM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Tuple.Extra (swap)
import Relude.Unsafe (read)
import Text.ParserCombinators.Parsec (GenParser, char, many1, oneOf, parse)

-- Input parsing

input :: Int -> FilePath
input day = "input/" <> show day <> ".txt"

exampleInput :: Int -> FilePath
exampleInput day = "input/" <> show day <> "_example.txt"

exampleInputN :: Int -> Int -> FilePath
exampleInputN day n = "input/" <> show day <> "_example_" <> show n <> ".txt"

-- Parse input lines with the given Reader.
-- If any error occurs, fail.
readInput :: TR.Reader a -> FilePath -> IO [a]
readInput r path = do
  xs <- fmap r . lines <$> readFileText path
  let (ls, rs) = partitionEithers xs
  case ls of
    [] -> return (fst <$> rs)
    es -> error (show es)

-- Helper utility for running a parser on a Text path
parseInput :: GenParser Char () a -> FilePath -> IO a
parseInput parser path = readWithParser parser <$> readFile (toString path)

readWithParser :: GenParser Char () a -> String -> a
readWithParser parser body =
  case parse parser "[input]" body of
    Right x -> x
    Left e -> error (show e)

-- Typeclass helpers

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- Specific currying / conversions

toTuple2 :: [a] -> Maybe (a, a)
toTuple2 [a, b] = Just (a, b)
toTuple2 _ = Nothing

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
