module Day12 (part1, part2) where

region :: G ℤ² Char -> ℤ² -> (Set ℤ², ℤ, ℤ, Σ ℤ)
region g c =
  let go r Empty segs area perim = (r, area, perim, sides segs)
      go r (((∈ r) -> True) :<| cs) segs area perim = go r cs segs area perim
      go r (c :<| cs) segs area perim =
        let ns = [n | n <- neighs @4 c g, g |! n ≡ g |! c]
            segs' = [(n - c, [c]) | n <- vicinity @4 c, g |? n /= g |? c]
         in go (c |-> r) (cs >< ns) (segs >< segs') (area + 1) (perim + 4 - size ns)
      sides segs = ((measure <$@> unMap (mkWith (<>) segs)) <>!)
      measure orientation cs =
        Σ . size $ case abs orientation of
          (0, 1) -> groupBy (\(x, y) (x', y') -> y ≡ y' ∧ diff x x' ≡ 1) (sortOn swap cs)
          (1, 0) -> groupBy (\(x, y) (x', y') -> x ≡ x' ∧ diff y y' ≡ 1) (sort cs)
   in go ø (mk₁ c) ø 0 0

prices :: G ℤ² Char -> ℤ²
prices g =
  let go (null -> True) ps = ps
      go cs ps =
        let (r, area, perim, Σ sides) = region g (arbitrary cs)
         in go (cs \\ r) (bimap (+ area ⋅ perim) (+ area ⋅ sides) ps)
   in go (mk $ coords g) (0, 0)

(part1, part2) :: (ℤ, ℤ) = prices (readGrid $(aoc 12))
