module Day7 (part1, part2) where

(g, [s]) :: ("S^." ▦ ℤ², [ℤ²]) = ($(aoc 7) ⋯) ⥢ (⇲ (|?> (#S □)))

go :: ℤ² .->. (Set ℤ², ℤ)
go (x, y) =
  ((⊏⊐) <$> g |? (x, y) ? ' ')
    ?> ((== ' ') ->> pure ((∅), 1))
    : ( (== '^')
          ->> do
            (a, b) <- go .$. (x - 1, y + 1)
            (c, d) <- go .$. (x + 1, y + 1)
            pure ((x, y) |-> a ∪ c, b + d)
      )
    : ((∈ ['S', '.']) ->> go .$. (x, y + 1))
    : []

(part1, part2) :: ℤ × ℤ = (|.|) ⇱ run (go s)
