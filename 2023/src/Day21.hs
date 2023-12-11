module Day21 (part1, part2) where

import Adlude

-- parser :: Parser [Int]
-- parser = many1 (number <* eol) <* eof

-- line :: Parser Int
-- line = number

-- data Cell
--   = Empty
--   | Wall
--   deriving (Eq, Ord)

-- instance GridCell Cell where
--   charMap =
--     mkBimap
--       [ (Empty, ' '),
--         (Wall, '#')
--       ]

part1 :: Text
part1 =
  $(input 21)
    -- & parseWith parser
    -- & parseLinesWith line
    -- & readGrid
    & (<> "Part 1")

part2 :: Text
part2 = "Part 2"
