module Day11 (part1, part2) where

import Data.List (foldl', iterate, sortOn, take, (!!))
import Data.Map.Strict qualified as M
import Data.Sequence qualified as SQ
import Helper.TH (input)
import Helper.Util (eol, number, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (Parser, char, eof, sepBy, spaces, string, (<|>))
import Prelude hiding ((<|>))

data Monkey = Monkey (Seq Integer) (Integer -> Integer) (Integer -> Integer) Integer

parser :: Parser (Map Integer Monkey, Integer)
parser = do
  monkeyDs <- (monkeyD `sepBy` eol) <* eof
  return (M.fromList $ fst <$> monkeyDs, product $ snd <$> monkeyDs)
  where
    monkeyD = do
      mId <- string "Monkey " *> number <* (string ":" >> eol)
      items <- SQ.fromList <$> (string "  Starting items: " *> number `sepBy` string ", ") <* eol
      operation <- do
        op <- string "  Operation: new = old " *> ((char '+' $> (+)) <|> (char '*' $> (*))) <* spaces
        (op <$> (number <* eol)) <|> ((string "old" >> eol) $> (\x -> op x x))
      (test, modulus) <- do
        modulus <- string "  Test: divisible by " *> number <* eol
        a <- string "    If true: throw to monkey " *> number <* eol
        b <- string "    If false: throw to monkey " *> number <* eol
        return (\x -> if x `mod` modulus == 0 then a else b, modulus)
      return ((mId, Monkey items operation test 0), modulus)

runTurn :: Integer -> Integer -> Map Integer Monkey -> Integer -> Map Integer Monkey
runTurn d modulus monkeys mId =
  case items of
    SQ.Empty -> monkeys
    (a SQ.:<| rest) ->
      let a' = (op a `div` d) `mod` modulus
          (destId, Monkey destItems destOp destTest dn) = (test a', monkeys M.! test a')
       in runTurn
            d
            modulus
            ( M.insert mId (Monkey rest op test (n + 1))
                . M.insert destId (Monkey (destItems SQ.:|> a') destOp destTest dn)
                $ monkeys
            )
            mId
  where
    (Monkey items op test n) = monkeys M.! mId

runRound :: Integer -> Integer -> Map Integer Monkey -> Map Integer Monkey
runRound d modulus monkeys =
  foldl'
    (runTurn d modulus)
    monkeys
    [0 .. fromIntegral $ M.size monkeys - 1]

solve :: Integer -> Int -> Integer
solve d rounds =
  let (monkeys, modulus) = parseWith parser $(input 11)
   in monkeys
        & iterate (runRound d modulus)
        & (!! rounds)
        & M.elems
        & fmap (\(Monkey _ _ _ n) -> n)
        & sortOn Down
        & take 2
        & product

part1 :: Integer
part1 = solve 3 20

part2 :: Integer
part2 = solve 1 10000
