module Day1 (part1, part2) where

lists :: [ℤ] × [ℤ]
lists = $(input 1) |-.. twoOf (spaceTabs `surrounding` number) & unzip

part1 :: ℤ
part1 = Σ ˙ sort `both` lists ⤊ diff

part2 :: ℤ
part2 = Σ ˙ counts <$> lists &<@> (*) <.> ((⇄ (|?)) .>. (? 0))
