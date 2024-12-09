module Day9 (part1, part2) where

defragment :: ([(ℤ, (ℤ, ℤ))] -> [(ℤ, (ℤ, ℤ))]) -> ℤ
defragment by =
  let freeOcc = ('0' : $(aoc 9)) |- many (twoOf (fromIntegral ∘ digitToInt <$> digit))
      scoreRange i (a, b) = i ⋅ (triangular b - triangular (a - 1))
      (ends, files, _) =
        foldl'
          ( \(ends, files, block) (i, (free, occ)) ->
              ( if free > 0 then (block, block + free - 1) : ends else ends,
                (i, (block + free, block + free + occ - 1)) : files,
                block + free + occ
              )
          )
          ([], [], 0)
          (zip [0 ..] freeOcc)
      (_, checksum) =
        foldl'
          ( \(ends, score) (i, r@(fa, fb)) ->
              let blocks = fb - fa + 1
                  go [] = (r, [])
                  go ((a, b) : rest)
                    | a > fa = (r, (a, b) : rest)
                    | a + blocks - 1 ≡ b = ((a, b), sort (r : rest))
                    | a + blocks - 1 < b = ((a, a + blocks - 1), sort (r : (a + blocks, b) : rest))
                    | otherwise = second ((a, b) :) (go rest)
                  (r', ends') = go ends
               in (ends', score + scoreRange i r')
          )
          (reverse ends, 0)
          (by files)
   in checksum

part1 :: ℤ
part1 = defragment $ \files -> [(i, (block, block)) | (i, (a, b)) <- files, block <- [a .. b]]

part2 :: ℤ
part2 = defragment id
