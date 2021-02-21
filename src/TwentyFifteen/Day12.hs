module TwentyFifteen.Day12 where

import Text.ParserCombinators.Parsec
  ( GenParser,
    between,
    char,
    eof,
    letter,
    many,
    many1,
    oneOf,
    sepBy,
    (<|>),
  )
import Util (eol, input, readWithParser)

data Json
  = Obj [(Json, Json)]
  | Lst [Json]
  | StrLit String
  | NumLit Int
  deriving (Eq, Ord, Show)

json :: GenParser Char () Json
json = obj <|> lst <|> strLit <|> numLit
  where
    numLit = NumLit . read <$> many (oneOf "-0123456789")
    strLit = StrLit <$> between (char '"') (char '"') (many1 letter)
    lst = Lst <$> between (char '[') (char ']') (json `sepBy` char ',')
    obj = Obj <$> between (char '{') (char '}') (keyVal `sepBy` char ',')
    keyVal = do
      k <- strLit
      char ':'
      v <- json
      return (k, v)

sumJson :: Json -> Int
sumJson (StrLit _) = 0
sumJson (NumLit x) = x
sumJson (Lst xs) = sum $ sumJson <$> xs
sumJson (Obj kvs) = sum $ sumJson . snd <$> kvs

solve :: (Json -> Int) -> IO Int
solve f = f . readWithParser (json <* eol <* eof) <$> input 2015 12

part1 :: IO Int
part1 = solve sumJson

noRedSum :: Json -> Int
noRedSum (StrLit _) = 0
noRedSum (NumLit x) = x
noRedSum (Lst xs) = sum $ noRedSum <$> xs
noRedSum (Obj kvs)
  | (StrLit "red") `elem` (snd <$> kvs) = 0
  | otherwise = sum $ noRedSum . snd <$> kvs

part2 :: IO Int
part2 = solve noRedSum
