module Day2 (part1, part2) where

import Text.Parsec.Combinator

safe :: 𝔹 -> Parser (Sum ℤ')
safe skip = go skip []
  where
    go skip levels =
      try (go skip . (: levels) =<< level levels)
        <|> (guard skip >> try (level [] >> go False levels))
        <|> (lookAhead eof $> Sum 1)
    level [] = wordOf $ number @ℤ'
    level ls = do
      l <- level []
      let (d : ds) = diffs (l : ls)
      l <$ guard (all ((`elem` [1, 2, 3]) . (* sgn d)) (d : ds))

part1 :: ℤ'
part1 = $(input 2) |-<> safe False

part2 :: ℤ'
part2 = $(input 2) |-<> safe True
