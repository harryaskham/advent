module Day14 (part1, part2) where

(dims@(w, h), robots) :: (ℤ², [ℤ⁴]) = ((101, 103), $(aoc 14) |-. tuples @4 (numbers @ℤ))

step :: ℤ⁴ -> ℤ⁴
step (x, y, vx, vy) = ((x + vx) `mod` w, (y + vy) `mod` h, vx, vy)

part1 :: ℤ
part1 =
  product $
    foldl'
      ( \[ul, ur, dl, dr] (x, y) ->
          case (x < w `div` 2, y < h `div` 2) of
            (True, True) -> [ul + 1, ur, dl, dr]
            (True, False) -> [ul, ur, dl + 1, dr]
            (False, True) -> [ul, ur + 1, dl, dr]
            (False, False) -> [ul, ur, dl, dr + 1]
      )
      [0, 0, 0, 0]
      [ (x, y)
        | (x, y, _, _) <- foldl' (\rs _ -> step <$> rs) robots [1 .. 100],
          (∧) $@ bimap (x ≢) (y ≢) (both (`div` 2) dims)
      ]

findTree :: ℝ -> Map ℤ ℝ -> ℝ -> ℝ -> ℤ -> [ℤ⁴] -> ℤ
findTree threshold q qn kl n rs =
  let kl' = kl0 q qn rs
   in if diff kl kl' >= threshold
        then n
        else findTree threshold q qn kl' (n + 1) (step <$> rs)

kl0 :: Map ℤ ℝ -> ℝ -> [ℤ⁴] -> ℝ
kl0 q qn rs =
  let p = as @ℝ @ℤ <$> counts [x + y | (x, y, _, _) <- rs]
      pn = as @ℝ $ length rs
   in sum
        [ kl px pn qx qn + kl qx qn px pn
          | (d, px) <- unMap p,
            let qx = q |! d,
            qx ≢ 0,
            let kl ax an bx bn = (ax / an) ⋅ logBase (exp 1) (bn ⋅ ax / an ⋅ bx)
        ]

part2 :: ℤ
part2 =
  let q = as @ℝ @ℤ <$> counts [x + y | x <- [0 .. w - 1], y <- [0 .. h - 1]]
      qn = w ⋅ h
   in findTree 0.6 q (qn & as @ℝ) (kl0 q (qn & as @ℝ) robots) 0 robots
