{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import Data.Typeable (Typeable)
import qualified Language.Haskell.Interpreter as Hint
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
