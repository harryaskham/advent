module Day9 where

(out, rects) :: [ℤ² × ℤ²] :^ 2 =
  (($(aoc 9) |- (⋮) @([ℤ ⹉ 2] ≠ [])) ⊏)
    & (outside ∘ loopPairs &&& triPairs)

part1 :: ℤ = ((Ȟ ((ds² <$@> rects) 🎝)) !>)

part2 :: ℤ = ((Ȟ ([ds² $@ r | r <- rects, not (or (intersectRectangles r <$> out))] 🎝)) !>)
