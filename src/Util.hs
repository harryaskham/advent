{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Typeable (Typeable)
import Debug.Trace (trace, traceShow)
import qualified Language.Haskell.Interpreter as Hint
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec (GenParser, char, parse)

input :: Int -> Int -> IO String
input year day = readFile $ "input/" ++ show year ++ "/" ++ show day ++ ".txt"

exampleInput :: Int -> Int -> IO String
exampleInput year day = readFile $ "input/" ++ show year ++ "/" ++ show day ++ "_example.txt"

exampleInputN :: Int -> Int -> Int -> IO String
exampleInputN year day n = readFile $ "input/" ++ show year ++ "/" ++ show day ++ "_example_" ++ show n ++ ".txt"

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

md5String :: String -> String
md5String = BC.unpack . B16.encode . MD5.hash . BC.pack

tracePause :: String -> a -> a
tracePause s a = unsafePerformIO $ do
  putStrLn s
  getLine
  return a

traceStrLn :: String -> a -> a
traceStrLn s a = unsafePerformIO $ do
  putStrLn s
  -- getLine
  return a

traceWhen :: Bool -> String -> a -> a
traceWhen p s a = if p then trace s a else a

traceStrLnWhen :: Bool -> String -> a -> a
traceStrLnWhen p s a
  | p = unsafePerformIO $ do
    putStrLn s
    return a
  | otherwise = a

fst4 (a, _, _, _) = a

snd4 (_, a, _, _) = a

thd4 (_, _, a, _) = a

fth4 (_, _, _, a) = a

first3 f (a, b, c) = (f a, b, c)
