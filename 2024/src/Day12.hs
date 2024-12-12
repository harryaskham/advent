module Day12 (part1, part2) where

region :: ℤ² Char -> (ℤ × ℤ) -> (Set (ℤ × ℤ), ℤ, ℤ, ℤ)
region g c = go (mk₁ c) ø 0 0 ø
  where
    go Empty r area perim segments = (r, area, perim, sides segments)
    go (c :<| cs) r area perim segments
      | c ∈ r = go cs r area perim segments
      | otherwise =
          let ns = [n | n <- neighs @4 c g, g |! n ≡ g |! c]
              segments' = [(n - c, [c]) | n <- vicinity @4 c, g |? n /= g |? c]
           in go (cs >< ns) (c |-> r) (area + 1) (perim + 4 - size ns) (segments >< segments')
    sides segments = size $ mconcat $ measure <$@> (unMap (mkWith (<>) segments))
    measure orientation cs =
      case abs orientation of
        (0, 1) -> groupBy (\(x, y) (x', y') -> y ≡ y' ∧ diff x x' ≡ 1) (sortOn swap cs)
        (1, 0) -> groupBy (\(x, y) (x', y') -> x ≡ x' ∧ diff y y' ≡ 1) (sort cs)

prices :: ℤ² Char -> (ℤ, ℤ)
prices g = go (mk $ coords g) 0 0
  where
    go cs perimPrice sidePrice
      | null cs = (perimPrice, sidePrice)
      | otherwise =
          let c = arbitrary cs
              (r, area, perim, sides) = region g c
           in go (cs \\ r) (perimPrice + area ⋅ perim) (sidePrice + area ⋅ sides)

part1 :: ℤ
part1 = $(aoc 12) & readGrid & prices & fst

part2 :: ℤ
part2 = $(aoc 12) & readGrid & prices & snd
