module Day21 (part1, part2) where

import Data.Array qualified as A
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Mod
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read
import Data.Vector qualified as V
import Helper.Coord
import Helper.Grid
import Helper.TH
import Helper.Tracers
import Helper.Util hiding (count)
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec
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
      name <- count 4 anyChar
      string ": "
      monkey <- try constMonkey <|> try rootMonkey <|> try opMonkey
      return (name, monkey)
    constMonkey = ConstMonkey <$> number
    opMonkey = do
      nameA <- count 4 anyChar
      op <- try (string " + " $> (+)) <|> try (string " - " $> (-)) <|> try (string " * " $> (*)) <|> try (string " / " $> div)
      nameB <- count 4 anyChar
      return $ OpMonkey (\ms -> value ms (ms M.! nameA) `op` value ms (ms M.! nameB))
    rootMonkey = do
      nameA <- count 4 anyChar
      op <- try (string " + " $> (+)) <|> try (string " - " $> (-)) <|> try (string " * " $> (*)) <|> try (string " / " $> div)
      nameB <- count 4 anyChar
      return $
        RootMonkey
          (\ms -> value ms (ms M.! nameA) `op` value ms (ms M.! nameB))
          ( \ms ->
              let a = value ms (ms M.! nameA)
                  b = value ms (ms M.! nameB)
               in compare a b
          )

part1 :: Integer
part1 =
  $(exampleInput 21)
    & parseWith parser
    & (\ms -> let root = ms M.! "root" in value ms root)

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
    (a', b') =
      case vM of
        EQ -> (a, b)
        LT -> (a, m)
        GT -> (m, b)

part2 :: Integer
part2 =
  $(input 21)
    & parseWith parser
    & (\ms -> search ms 0 10000000000000)
