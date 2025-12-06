module Day6 (part1, part2) where

(part1, part2) :: ℕ × ℕ =
  both
    (sum . ((foldl1 $@) <$>) . (zip $@) . swap)
    ( first transpose ($(aoc 6) ⋯),
      let Just s = nonEmpty (unpack <$> lines $(aoc 6))
       in ( fmap (|- ((many (char ' ')) *> (nat `endBy` many (char ' '))))
              . (intercalate " " <$>)
              . splitBy (all (== ' '))
              . transpose
              $ init s,
            ((many (char ' ')) *> mathOp `endBy` many (char ' ')) -| last s
          )
    )
