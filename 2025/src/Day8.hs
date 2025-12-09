module Day8 (part1, part2) where

boxes :: [ℤ³] = (($(aoc 8) |- parseVia @([CSV ℤ 3] ≠ [])) ⊏)

ps :: [ℤ³ × ℤ³] = sortOn (ssd³ $@) (triPairs boxes)

circuits :: Set (Set ℤ³) = mk (mk . pure <$> boxes)

connect :: Set (Set ℤ³) -> ℤ³ × ℤ³ -> Set (Set ℤ³)
connect c (a, b) =
  let (aC, bC) = both (head' . (<-?-| c) . (∈)) (a, b)
      c' = aC ∪ bC |-> c |\ aC |\ bC
   in aC == bC ??? c $ c'

final :: Set (Set ℤ³) -> [ℤ³ × ℤ³] -> (ℤ³, ℤ³)
final circuits (p : ps) =
  let circuits' = connect circuits p
   in (circuits' |.|) == 1 ??? p $ final circuits' ps

part1 :: Π ℤ = ((Π <$> take 3 ((((Ł connect circuits (take 1000 ps) !>) ⊏) <&> (|.|)) 🎝)) <>!)

part2 :: ℤ = (*) $@ both fst3 (final circuits ps)
