module TwentyFifteen.Day7 where

import Control.Monad.Memo (Memo, MonadMemo (memo), startEvalMemo)
import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
  ( GenParser,
    choice,
    eof,
    letter,
    many,
    many1,
    string,
    try,
    (<|>),
  )
import Util (eol, input, number, readWithParser)

data Connection
  = Assign Value Value
  | And Value Value Value
  | Or Value Value Value
  | Not Value Value
  | LShift Value Int Value
  | RShift Value Int Value

data Value = Wire String | Literal Int deriving (Eq, Ord)

connections :: GenParser Char () [Connection]
connections = many instruction <* eof
  where
    instruction = choice (try <$> [assign, and, or, not, lshift, rshift]) <* eol
    wire = Wire <$> many1 letter
    literal = Literal <$> number
    value = wire <|> literal
    assign = Assign <$> value <* string " -> " <*> wire
    and = And <$> value <* string " AND " <*> value <* string " -> " <*> wire
    or = Or <$> value <* string " OR " <*> value <* string " -> " <*> wire
    not = Not <$> (string "NOT " >> value) <* string " -> " <*> wire
    lshift = LShift <$> value <* string " LSHIFT " <*> number <* string " -> " <*> wire
    rshift = RShift <$> value <* string " RSHIFT " <*> number <* string " -> " <*> wire

outputWire :: Connection -> Value
outputWire (Assign _ v) = v
outputWire (And _ _ v) = v
outputWire (Or _ _ v) = v
outputWire (Not _ v) = v
outputWire (LShift _ _ v) = v
outputWire (RShift _ _ v) = v

wireMap :: [Connection] -> Map Value Connection
wireMap cs = M.fromList [(outputWire c, c) | c <- cs]

resolve :: Map Value Connection -> Value -> Int
resolve wm wire = startEvalMemo $ go wire
  where
    go :: Value -> Memo Value Int Int
    go (Literal x) = return x
    go wire =
      case wm M.! wire of
        Assign v _ -> memo go v
        And a b _ -> (.&.) <$> memo go a <*> memo go b
        Or a b _ -> (.|.) <$> memo go a <*> memo go b
        Not a _ -> complement <$> memo go a
        LShift a n _ -> shiftL <$> memo go a <*> pure n
        RShift a n _ -> shiftR <$> memo go a <*> pure n

part12 :: IO (Int, Int)
part12 = do
  wm <- wireMap . readWithParser connections <$> input 2015 7
  let part1 = resolve wm (Wire "a")
      part2 =
        resolve
          (M.insert (Wire "b") (Assign (Literal part1) (Wire "b")) wm)
          (Wire "a")
  return (part1, part2)
