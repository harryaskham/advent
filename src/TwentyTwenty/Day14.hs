module TwentyTwenty.Day14 where

import Data.Char (digitToInt)
import Data.Either (fromRight)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
  ( GenParser,
    alphaNum,
    char,
    count,
    digit,
    eof,
    many,
    many1,
    parse,
    string,
    try,
    (<|>),
  )
import Text.Printf (printf)

inputPath :: String
inputPath = "input/2020/14.txt"

data MaskBit = M1 | M0 | MX

type Mask = [MaskBit]

data ValueBit = V1 | V0

newtype DecimalValue = DecimalValue Int

type Value = [ValueBit]

newtype Address = Address Int deriving (Eq, Ord)

data Instruction = SetMask Mask | SetMem Address DecimalValue

type Memory = M.Map Address Value

data Machine = Machine Memory Mask [Instruction]

maskBitFromChar :: Char -> MaskBit
maskBitFromChar '1' = M1
maskBitFromChar '0' = M0
maskBitFromChar 'X' = MX

valueBitFromChar :: Char -> ValueBit
valueBitFromChar '1' = V1
valueBitFromChar '0' = V0

valueToInt :: Value -> Int
valueToInt v = foldl' (\acc x -> acc * 2 + x) 0 $ toInt <$> v
  where
    toInt V1 = 1
    toInt V0 = 0

applyMask :: Mask -> Value -> Value
applyMask mask value = uncurry applyBit <$> zip mask value
  where
    applyBit MX v = v
    applyBit M1 _ = V1
    applyBit M0 _ = V0

toB36 :: Int -> Value
toB36 x = valueBitFromChar <$> printf "%036b" x

parseInstructions :: GenParser Char st [Instruction]
parseInstructions = do
  instructions <- many instruction
  eof
  return instructions
  where
    eol = char '\n'
    instruction = do
      is <- try setMask <|> try setMem
      eol
      return is
    setMask = do
      string "mask = "
      mask <- count 36 alphaNum
      return $ SetMask (maskBitFromChar <$> mask)
    setMem = do
      string "mem["
      address <- many1 digit
      string "] = "
      value <- many1 digit
      return $
        SetMem
          (Address (read address))
          (DecimalValue $ read value)

readInstructions :: IO [Instruction]
readInstructions =
  fromRight []
    . parse parseInstructions "[input]"
    <$> readFile inputPath

mkMachine :: [Instruction] -> Machine
mkMachine = Machine M.empty []

type MemUpdater = Memory -> DecimalValue -> Mask -> Address -> Memory

memUpdateV1 :: MemUpdater
memUpdateV1 memory (DecimalValue dVal) mask address =
  M.insert address (applyMask mask $ toB36 dVal) memory

runMachine :: MemUpdater -> Machine -> Machine
runMachine _ machine@(Machine _ _ []) = machine
runMachine memUpdater (Machine memory mask (i : is)) =
  case i of
    SetMask mask' -> runMachine memUpdater $ Machine memory mask' is
    SetMem address dVal ->
      let memory' = memUpdater memory dVal mask address
       in runMachine memUpdater $ Machine memory' mask is

solve :: MemUpdater -> IO Int
solve memUpdater = do
  (Machine memory _ _) <-
    runMachine memUpdater . mkMachine <$> readInstructions
  return $ sum (valueToInt . snd <$> M.toList memory)

part1 :: IO Int
part1 = solve memUpdateV1

applyFloatingMask :: Mask -> Address -> [Address]
applyFloatingMask mask (Address address) =
  Address . valueToInt . reverse
    <$> foldl'
      (\acc b -> (:) <$> b <*> acc)
      [[]]
      (uncurry applyBit <$> zip mask (toB36 address))
  where
    applyBit M1 _ = [V1]
    applyBit M0 v = [v]
    applyBit MX _ = [V0, V1]

memUpdateV2 :: MemUpdater
memUpdateV2 memory (DecimalValue dVal) mask address =
  foldl'
    (\mem add -> M.insert add (toB36 dVal) mem)
    memory
    (applyFloatingMask mask address)

part2 :: IO Int
part2 = solve memUpdateV2
