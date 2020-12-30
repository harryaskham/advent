module TwentySixteen.Day7 where

import qualified Data.Set as S
import Data.Tuple.Extra (both)
import Text.ParserCombinators.Parsec
  ( GenParser,
    between,
    char,
    eof,
    letter,
    many,
    many1,
    try,
    (<|>),
  )
import Util (eol, readWithParser)

inputPath :: String
inputPath = "input/2016/7.txt"

data BlockType = Inside | Outside deriving (Eq)

addresses :: GenParser Char () [[(String, BlockType)]]
addresses = do
  as <- many address
  eof
  return as
  where
    address = do
      bs <- many1 (try innerBlock <|> try outerBlock)
      eol
      return bs
    outerBlock = do
      b <- many1 letter
      return (b, Outside)
    innerBlock = do
      b <- between (char '[') (char ']') (many1 letter)
      return (b, Inside)

containsAbba :: String -> Bool
containsAbba [_, _, _] = False
containsAbba (a1 : b1 : b2 : a2 : xs) =
  (a1 == a2 && b1 == b2 && a1 /= b1) || containsAbba (b1 : b2 : a2 : xs)

supportsTLS :: Int -> [(String, BlockType)] -> Bool
supportsTLS numAbbas [] = numAbbas > 0
supportsTLS numAbbas ((s, t) : bs) =
  case t of
    Outside ->
      if containsAbba s
        then supportsTLS (numAbbas + 1) bs
        else supportsTLS numAbbas bs
    Inside -> not (containsAbba s) && supportsTLS numAbbas bs

readAddresses :: IO [[(String, BlockType)]]
readAddresses = readWithParser addresses <$> readFile inputPath

part1 :: IO Int
part1 = length . filter (supportsTLS 0) <$> readAddresses

getAbas :: S.Set String -> String -> S.Set String
getAbas abas [_, _] = abas
getAbas abas (a1 : b : a2 : xs)
  | a1 == a2 && a1 /= b = getAbas (S.insert [a1, b, a2] abas) (b : a2 : xs)
  | otherwise = getAbas abas (b : a2 : xs)

babToAba :: String -> String
babToAba (b : a : _) = [a, b, a]

supportsSSL :: [(String, BlockType)] -> Bool
supportsSSL a = not $ S.null (abas `S.intersection` S.map babToAba babs)
  where
    [insides, outsides] = (\t -> filter ((== t) . snd) a) <$> [Inside, Outside]
    allAbas xs = foldl1 S.union $ getAbas S.empty <$> (fst <$> xs)
    (abas, babs) = both allAbas (outsides, insides)

part2 :: IO Int
part2 = length . filter supportsSSL <$> readAddresses
