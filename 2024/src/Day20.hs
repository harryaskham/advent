module Day20 (part1, part2) where

cheats :: ".#SE" ▦ ℤ² -> ℤ -> ℤ -> [Σ ℤ]
cheats g duration threshold =
  [ Σ 1
    | let path = go (g |!> (#E □)) [g |!> (#S □)] ø,
      let c2n = mkMap (zip path [0 ..]),
      (d, c) <- zip [0 ..] (reverse path),
      n <- drop (d + 1) (reverse path),
      let d' = manhattan c n,
      d' <= duration,
      let cost = d + d' + c2n |! n,
      size path - cost >= threshold
  ]
  where
    go end (c : cs) seen
      | c ≡ end = c : cs
      | otherwise =
          let [n] = [n | n <- neighs @4 c g, g |! n ≢ (mkC @'#'), n ∉ seen]
           in go end (n : c : cs) (c |-> seen)

part1 :: Σ ℤ
part1 = (cheats (readGrid $(aoc 20)) 2 100 <>!)

part2 :: Σ ℤ
part2 = (cheats (readGrid $(aoc 20)) 20 100 <>!)
