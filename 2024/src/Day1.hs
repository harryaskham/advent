module Day1 (part1, part2) where

lists :: [ℤ₆₄] × [ℤ₆₄]
lists = $(input 1) |-.. twoOf (spaceTabs `surrounding` number) & unzip

part1 :: ℤ₆₄
part1 = Σ ˙ sort `both` lists ⤊ diff

part2 :: ℤ₆₄
part2 = Σ ˙ counts <$> lists &<@> (*) <.> (? 0) .<. (⇄ (|?))
