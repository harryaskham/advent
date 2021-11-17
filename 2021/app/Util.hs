{-# LANGUAGE TupleSections #-}

module Util where

import Control.Monad (filterM)
import Control.Monad.State.Lazy
import Data.List.Extra (groupOn, sortOn, stripInfix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Ord (Down (Down))
import Data.Tuple.Extra (swap)
import Text.ParserCombinators.Parsec (GenParser, char, many1, oneOf, parse)

-- Input parsing

input :: Int -> IO String
input day = readFile $ "input/" ++ show day ++ ".txt"

exampleInput :: Int -> IO String
exampleInput day = readFile $ "input/" ++ show day ++ "_example.txt"

exampleInputN :: Int -> Int -> IO String
exampleInputN day n = readFile $ "input/" ++ show day ++ "_example_" ++ show n ++ ".txt"

-- Typeclass helpers

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 5 <***>

(<***>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> g a -> f (g b)
f <***> a = (<*> a) <$> f

-- Specific currying / conversions

toTuple2 :: [a] -> (a, a)
toTuple2 [a, b] = (a, b)

toTuple3 :: [a] -> (a, a, a)
toTuple3 [a, b, c] = (a, b, c)

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f = let f' (a, b, c, d) = f a b c d in f'

fst4 (a, _, _, _) = a

snd4 (_, a, _, _) = a

thd4 (_, _, a, _) = a

fth4 (_, _, _, a) = a

first3 f (a, b, c) = (f a, b, c)

-- Parser Combinator Utils

readWithParser :: GenParser Char () a -> String -> a
readWithParser parser input =
  case parse parser "[input]" input of
    Right x -> x
    Left e -> error (show e)

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

swapMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
swapMap = M.fromList . fmap swap . M.toList

-- List helpers

maxIndices :: Ord a => [a] -> [Int]
maxIndices = fmap fst . head . groupOn snd . sortOn (Down . snd) . zip [0 ..]

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- Memoization

memoize :: Ord a => (a -> b) -> ((Map a b, a) -> (Map a b, b))
memoize f (m, a) =
  case M.lookup a m of
    Just b -> (m, b)
    Nothing -> let b = f a in (M.insert a b m, b)

fibM :: (Map Int Int, Int) -> (Map Int Int, Int)
fibM (m, 1) = (m, 1)
fibM (m, 2) = (m, 1)
fibM (m, a) = case M.lookup a m of
  Just b -> (m, b)
  Nothing ->
    let (m1, f1) = fibM (m, a - 1)
        (m2, f2) = fibM (m1, a - 2)
        b = f1 + f2
     in (M.insert a b m2, b)

fib n = snd $ fibM (M.empty, n)
