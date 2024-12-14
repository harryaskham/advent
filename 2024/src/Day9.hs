module Day9 (part1, part2) where

defragment :: ([(ℤ, (ℤ, ℤ))] -> [(ℤ, (ℤ, ℤ))]) -> ℤ
defragment by =
  ('0' : $(aoc 9))
    |- many (twoOf (as @ℤ ∘ digitToInt <$> digit))
    & (..#)
    & foldl'
      ( \(ends, files, start) (i, (free, full)) ->
          ( free > 0 ??? ends $ ends |. (start, start + free - 1),
            (i, (start + free, start + free + full - 1)) : files,
            start + free + full
          )
      )
      (ø, ø, 0)
    & (\(a, b, _) -> (a, b))
    & (bimap (,0) by)
    &@ foldl'
      ( \(ends, score) (i, r) ->
          second
            ((+ score) ∘ (⋅ i) ∘ ((-) $@) ∘ (triangular <:>) ∘ second (subtract 1) ∘ swap)
            (place r ends)
      )
    & snd
  where
    place r@(fa, fb) ends
      | nullQ ends = (ø, r)
      | a > fa = (ends' |. (a, b), r)
      | a - fa + fb ≡ b = (ends' |. r, (a, b))
      | a - fa + fb < b = (ends' |. r |. (a - fa + fb + 1, b), (a, a - fa + fb))
      | otherwise = first (|. (a, b)) (place r ends')
      where
        ((a, b), ends') = (ends <!)

part1 :: ℤ
part1 = defragment $ \files -> [(i, (block, block)) | (i, (a, b)) <- files, block <- [a .. b]]

part2 :: ℤ
part2 = defragment id
