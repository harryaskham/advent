{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import qualified Data.Map.Strict as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Typeable (Typeable)
import qualified Language.Haskell.Interpreter as Hint
import Text.ParserCombinators.Parsec (GenParser, char, parse)

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

toTuple2 :: [a] -> (a, a)
toTuple2 [a, b] = (a, b)

toTuple3 :: [a] -> (a, a, a)
toTuple3 [a, b, c] = (a, b, c)

readWithParser :: GenParser Char () a -> String -> a
readWithParser parser input = do
  case parse parser "[input]" input of
    Right x -> x
    Left e -> error (show e)

eol :: GenParser Char () Char
eol = char '\n'

-- Run a set of expressions of the same type using Hint mixins.
-- Expressions have HintMixins.hs in scope.
eval :: forall t. Typeable t => [String] -> IO [t]
eval exprs = do
  result <- Hint.runInterpreter $ do
    Hint.loadModules ["src/HintMixins.hs"]
    Hint.setTopLevelModules ["HintMixins"]
    Hint.setImports ["Prelude"]
    sequence $ Hint.interpret <$> exprs <*> pure (Hint.as :: t)
  case result of
    Right a -> return a
    Left e -> error (show e)

countMap :: Ord a => [a] -> M.Map a Int
countMap xs = getSum <$> M.fromListWith (+) (zip xs (repeat $ Sum 1))

adjustWithDefault :: Ord k => a -> (a -> a) -> k -> M.Map k a -> M.Map k a
adjustWithDefault def f k m = case M.lookup k m of
  Nothing -> M.insert k (f def) m
  Just a -> M.insert k (f a) m
