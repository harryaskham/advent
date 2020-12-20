{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import Data.Function (fix)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import Data.Typeable (Typeable)
import qualified Language.Haskell.Interpreter as Hint
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec (GenParser, parse)

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

readWithParser :: GenParser Char () a -> String -> a
readWithParser parser input = do
  case parse parser "[input]" input of
    Right x -> x
    Left e -> error (show e)

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

memoizeWithKey :: Ord k => (a -> k) -> (a -> b) -> (a -> b)
memoizeWithKey memoKey f = doUnsafe $ do
  cacheRef <- newIORef M.empty
  let f' x = do
        cache <- readIORef cacheRef
        let key = memoKey x
        case M.lookup key cache of
          Nothing -> do
            let v = f x
            modifyIORef' cacheRef (M.insert key v)
            return v
          Just v -> return v
  return f'

doUnsafe :: IO (a -> IO b) -> (a -> b)
doUnsafe f =
  let f' = unsafePerformIO f
   in \x -> unsafePerformIO (f' x)

memoize :: Ord a => (a -> b) -> (a -> b)
memoize = memoizeWithKey id
