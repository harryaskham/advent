module TwentySixteen.Day12 where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    choice,
    eof,
    letter,
    many,
    many1,
    oneOf,
    string,
    try,
    (<|>),
  )
import Util (eol, input, readWithParser)

data Value = Register Char | Literal Int deriving (Show)

data Instruction
  = Cpy Value Value
  | Inc Char
  | Dec Char
  | Jnz Value Value
  | Tgl Value
  | Out Value
  deriving (Show)

coerce :: Map Char Int -> Value -> Int
coerce _ (Literal x) = x
coerce mem (Register c) = mem M.! c

instructions :: GenParser Char () [Instruction]
instructions = do
  is <- many instruction
  eof
  return is
  where
    instruction = choice $ try <$> [cpy, inc, dec, jnz, tgl, out]
    literal = read <$> many1 (oneOf "-0123456789")
    value = (Register <$> letter) <|> (Literal <$> literal)
    cpy = do
      string "cpy "
      x <- value
      char ' '
      y <- value
      eol
      return $ Cpy x y
    inc = do
      string "inc "
      a <- letter
      eol
      return $ Inc a
    dec = do
      string "dec "
      a <- letter
      eol
      return $ Dec a
    jnz = do
      string "jnz "
      x <- value
      char ' '
      y <- value
      eol
      return $ Jnz x y
    tgl = do
      string "tgl "
      x <- value
      eol
      return $ Tgl x
    out = do
      string "out "
      x <- value
      eol
      return $ Out x

data Machine = Machine Int (Map Char Int)

mkMachine :: Machine
mkMachine = Machine 0 (M.fromList $ zip ['a' .. 'd'] (repeat 0))

step :: Instruction -> Vector Instruction -> Machine -> (Vector Instruction, Machine, Maybe Int)
step i is m@(Machine pc mem) =
  case i of
    (Cpy v r) -> case r of
      Literal _ -> error "Invalid CPY"
      Register r -> (is, Machine (pc + 1) (M.insert r (coerce mem v) mem), Nothing)
    (Inc r) -> (is, Machine (pc + 1) (M.adjust (+ 1) r mem), Nothing)
    (Dec r) -> (is, Machine (pc + 1) (M.adjust (subtract 1) r mem), Nothing)
    (Jnz v inc) ->
      if coerce mem v == 0
        then (is, Machine (pc + 1) mem, Nothing)
        else (is, Machine (pc + coerce mem inc) mem, Nothing)
    (Tgl x) ->
      let tgl (Inc r) = Dec r
          tgl (Dec r) = Inc r
          tgl (Jnz v inc) = Cpy v inc
          tgl (Cpy v r) = Jnz v r
          tgl (Tgl (Register r)) = Inc r
          ix = (pc + coerce mem x)
       in case is V.!? ix of
            Nothing -> (is, Machine (pc + 1) mem, Nothing)
            Just i -> (is V.// [(ix, tgl i)], Machine (pc + 1) mem, Nothing)
    (Out x) -> (is, Machine (pc + 1) mem, Just (coerce mem x))

run :: Vector Instruction -> Machine -> (Vector Instruction, Machine)
run is m@(Machine pc _) =
  case is V.!? pc of
    Nothing -> (is, m)
    Just i -> let (is', m', _) = step i is m in run is' m'

part1 :: IO Int
part1 = do
  is <- readWithParser instructions <$> input 2016 12
  let (_, Machine _ mem) = run (V.fromList is) mkMachine
  return $ mem M.! 'a'

part2 :: IO Int
part2 = do
  is <- readWithParser instructions <$> input 2016 12
  let (_, Machine _ mem) =
        run
          (V.fromList is)
          (Machine 0 (M.fromList (zip ['a' .. 'd'] [0, 0, 1, 0])))
  return $ mem M.! 'a'
