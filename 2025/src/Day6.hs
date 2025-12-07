module Day6 (part1, part2) where

(ps, ops) :: [[ℕ]] × [ℕ -> ℕ -> ℕ] = ((↻) ⇱ ($(aoc 6) ⋯))

ps' :: [[ℕ]] = (((++ "\n") . drop 1 <$> (($(aoc 6) ⋯) ↻) <>!) ⋯)

-- ps' :: [[ℕ]] = ((⋯) <$>) . (intercalate " " <$>) . splitBy null . ((<>?) <$>) . (unℕc <$$>) $ psC

part1 :: Σ ℕ = (<>!) . ((Σ . (!>) . (Ŀ $@)) <$>) . (zip $@) . swap $ (ps, ops)

part2 :: Σ ℕ = (<>!) . ((Σ . (!>) . (Ŀ $@)) <$>) . (zip $@) . swap $ (ps', ops)
