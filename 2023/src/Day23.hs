module Day23 (part1, part2) where

-- parser :: Parser [Int]
-- parser = many1 (number <* eol) <* eof

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
  $(input 23)
    -- \$(grid input 23)
    -- \|- parser
    & (<> "Part 1")

part2 :: Text
part2 = "Part 2"
