module Day5 (part1, part2) where

import Data.List (minimum)
import Data.List.Extra (chunksOf)
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec (Parser, char, eof, many1, noneOf, sepBy1, string)

parser :: Parser ([Integer], [Integer], [(Integer -> Integer, Integer -> Integer)])
parser = do
  seeds <- string "seeds: " *> (number `sepBy1` string " ") <* (eol >> eol)
  (mappings, destinations) <- unzip <$> (mapping `sepBy1` eol) <* eof
  return (seeds, concat destinations, mappings)
  where
    rangeLine = do
      (dest, source) <- let n = (number <* char ' ') in (,) <$> n <*> n
      rangeSize <- number <* eol
      return $
        ( toTuple2 $
            [(source, dest), (dest, source)]
              <&> ( \(a, b) x ->
                      if (x >= a) && (x < a + rangeSize)
                        then Just $ b + (x - a)
                        else Nothing
                  ),
          dest
        )
    mapping = do
      many1 (noneOf "\n") >> eol
      (fs, destinations) <- unzip <$> many1 rangeLine
      let forwardBackward =
            toTuple2 $
              [fst, snd]
                <&> ( \f x -> case catMaybes (f <$> fs <*> pure x) of
                        [] -> x
                        (x' : _) -> x'
                    )
      return (forwardBackward, destinations)

part1 :: Integer
part1 =
  $(input 5)
    & parseWith parser
    & (\(seeds, _, mappings) -> flip (foldl' (flip fst)) mappings <$> seeds)
    & minimum

part2 :: Integer
part2 =
  $(input 5)
    & parseWith parser
    & ( \(seeds, destinations, mappings) ->
          let validSeed seed = or [(seed >= a) && (seed < a + b) | [a, b] <- chunksOf 2 seeds]
           in [d | d <- destinations, validSeed (foldr snd d mappings)]
      )
    & minimum
