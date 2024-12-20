module Day20 (part1, part2) where

cheats :: ".#SE" ▦ ℤ² -> ℤ -> ℤ -> ℤ
cheats g duration threshold =
  size
    [ (n, d + c2n |! n)
      | let path = go (mk₁ [start]) ø,
        let c2n = mkMap (zip (reverse path) [0 ..]),
        (i, c) <- zip [0 ..] path,
        n <- drop (i + 1) path,
        let d = manhattan c n,
        d <= duration,
        let cost = i + d + (c2n |! n),
        size path - cost >= threshold
    ]
  where
    (start, end) = both (g |!>) (mkC @'S' @".#SE", mkC @'E' @".#SE")
    go ((c : cs) :<| q) seen
      | c ≡ end = reverse (c : cs)
      | c ∈ seen = go q seen
      | otherwise =
          let q' = q >< mk [n : c : cs | n <- neighs @4 c g, g |! n ≢ (mkC @'#' @".#SE")]
           in go q' (c |-> seen)

part1 :: ℤ
part1 = cheats (readGrid $(aoc 20)) 2 100

part2 :: ℤ
part2 = cheats (readGrid $(aoc 20)) 20 100
