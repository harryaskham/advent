module Day21 (part1, part2) where

import Data.Map.Strict qualified as M
import Helper.TH (exampleInput, input)
import Helper.Util (eol, number, parseWith)
import Text.ParserCombinators.Parsec (Parser, anyChar, count, eof, many1, string, try, (<|>))
import Prelude hiding ((<|>))

type Monkeys = Map String Monkey

data Monkey = ConstMonkey Integer | OpMonkey (Monkeys -> Integer) | RootMonkey (Monkeys -> Integer) (Monkeys -> Ordering)

value :: Monkeys -> Monkey -> Integer
value _ (ConstMonkey x) = x
value ms (OpMonkey f) = f ms
value ms (RootMonkey f _) = f ms

parser :: Parser Monkeys
parser = M.fromList <$> many1 (monkey <* eol) <* eof
  where
    monkey = do
      name <- count 4 anyChar <* string ": "
      monkey <- try constMonkey <|> try rootMonkey <|> try opMonkey
      return (name, monkey)
    operation =
      try (string " + " $> (+))
        <|> try (string " - " $> (-))
        <|> try (string " * " $> (*))
        <|> try (string " / " $> div)
    args = do
      nameA <- count 4 anyChar
      op <- operation
      nameB <- count 4 anyChar
      return (nameA, op, nameB)
    constMonkey = ConstMonkey <$> number
    opMonkey = do
      (nameA, op, nameB) <- args
      return $ OpMonkey (\ms -> value ms (ms M.! nameA) `op` value ms (ms M.! nameB))
    rootMonkey = do
      (nameA, op, nameB) <- args
      return $
        RootMonkey
          (\ms -> value ms (ms M.! nameA) `op` value ms (ms M.! nameB))
          ( \ms ->
              let a = value ms (ms M.! nameA)
                  b = value ms (ms M.! nameB)
               in compare a b
          )

search :: Monkeys -> Integer -> Integer -> Integer
search ms a b
  | abs (a - b) <= 1 = b
  | vM == EQ = search ms a (b - 1)
  | otherwise = search ms a' b'
  where
    (RootMonkey _ p) = ms M.! "root"
    m = (a + b) `div` 2
    ms' = M.insert "humn" (ConstMonkey m) ms
    vM = p ms'
    (a', b') = case vM of
      EQ -> (a, b)
      LT -> (a, m)
      GT -> (m, b)

part1 :: Integer
part1 =
  $(input 21)
    & parseWith parser
    & (\ms -> let root = ms M.! "root" in value ms root)

part2 :: Integer
part2 =
  $(input 21)
    & parseWith parser
    & (\ms -> search ms 0 10000000000000)
