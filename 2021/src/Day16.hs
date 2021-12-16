module Day16 (part1, part2) where

import Data.List qualified as L
import Data.Text qualified as T
import Helper.Bits (bitsToInt, hexToBin)
import Helper.TH (input)
import Helper.Util (bitChar, nBitInt, parseWith, toTuple2)
import Text.ParserCombinators.Parsec (GenParser, count, oneOf)

newtype Version = Version Integer deriving (Eq)

data OpType
  = OpSum
  | OpProduct
  | OpMin
  | OpMax
  | OpGT
  | OpLT
  | OpEq
  deriving (Eq)

data Body
  = Literal Integer
  | Operator OpType [Packet]
  deriving (Eq)

data Packet = Packet Version Body deriving (Eq)

opFromInt :: Integer -> OpType
opFromInt 0 = OpSum
opFromInt 1 = OpProduct
opFromInt 2 = OpMin
opFromInt 3 = OpMax
opFromInt 5 = OpGT
opFromInt 6 = OpLT
opFromInt 7 = OpEq
opFromInt i = error ("Invalid op: " <> show i)

packet :: GenParser Char () Packet
packet = do
  version <- Version <$> nBitInt 3
  typeId <- nBitInt 3
  p <- case typeId of
    4 -> Packet version <$> literal
    opId -> Packet version <$> operator opId
  return p

literal :: GenParser Char () Body
literal = Literal . bitsToInt <$> groupedBin
  where
    groupedBin = do
      b : bs <- count 5 bitChar
      if b then (bs ++) <$> groupedBin else return bs

operator :: Integer -> GenParser Char () Body
operator opId =
  Operator (opFromInt opId) <$> do
    ltID <- bitChar
    if ltID then packetBody else lengthBody
  where
    packetBody = do
      numPackets <- nBitInt 11
      count (fromIntegral numPackets) packet
    lengthBody = do
      bitLength <- nBitInt 15
      packetBits <- count (fromIntegral bitLength) (oneOf "01")
      return $ parseWith (many packet) packetBits

versionSum :: Packet -> Integer
versionSum (Packet (Version v) (Literal _)) = v
versionSum (Packet (Version v) (Operator _ ps)) = v + sum (versionSum <$> ps)

eval :: Packet -> Integer
eval (Packet _ (Literal v)) = v
eval (Packet _ (Operator ot ps)) =
  let vs = eval <$> ps
      binOp f = fromIntegral . fromEnum . (f <$> fst <*> snd) . toTuple2 $ vs
   in case ot of
        OpSum -> sum vs
        OpProduct -> product vs
        OpMin -> L.minimum vs
        OpMax -> L.maximum vs
        OpGT -> binOp (>)
        OpLT -> binOp (<)
        OpEq -> binOp (==)

solve :: (Packet -> Integer) -> Integer
solve f =
  $(input 16)
    & (T.unpack . L.head . T.lines)
    & (hexToBin =<<)
    & parseWith packet
    & f

part1 :: Integer
part1 = solve versionSum

part2 :: Integer
part2 = solve eval
