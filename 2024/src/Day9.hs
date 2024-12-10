module Day9 (part1, part2) where

defragment :: ([(ℤ, (ℤ, ℤ))] -> [(ℤ, (ℤ, ℤ))]) -> ℤ
defragment by =
  let freeFull = ('0' : $(aoc 9)) |- many (twoOf (fromIntegral ∘ digitToInt <$> digit))
      (ends, files, _) =
        foldl'
          ( \(ends, files, start) (i, (free, full)) ->
              ( free > 0 ??? ends $ ends |. (start, start + free - 1),
                (i, (start + free, start + free + full - 1)) : files,
                start + free + full
              )
          )
          (ø, ø, 0)
          (freeFull ..#)
      place r@(fa, fb) ends
        | nullQ ends = (ø, r)
        | a > fa = (ends' |. (a, b), r)
        | a + blocks - 1 ≡ b = (ends' |. r, (a, b))
        | a + blocks - 1 < b = (ends' |. r |. (a + blocks, b), (a, a + blocks - 1))
        | otherwise = first (|. (a, b)) (place r ends')
        where
          ((a, b), ends') = (ends <!)
          blocks = fb - fa + 1
   in snd $
        foldl'
          ( \(ends, score) (i, r) ->
              second
                ((+ score) ∘ (⋅ i) ∘ ((-) $@) ∘ (triangular <:>) ∘ second (subtract 1) ∘ swap)
                (place r ends)
          )
          (ends, 0)
          (by files)

part1 :: ℤ
part1 = defragment $ \files -> [(i, (block, block)) | (i, (a, b)) <- files, block <- [a .. b]]

part2 :: ℤ
part2 = defragment id
