module Day4 (part1, part2) where

removable :: ".@" ▦ ℤ² -> [ℤ²]
removable g =
  let cs =
        g
          & unGrid
          & fmap
            ( \(c, x) ->
                if x == (#"@" □) && size [n | n <- neighs @8 @[] c g, g |! n == (#"@" □)] < 4
                  then Just c
                  else Nothing
            )
          & mkSet
   in [c | c <- coords g, Just c ∈ cs]

remove :: ℤ -> ".@" ▦ ℤ² -> ℤ
remove 0 _ = 0
remove n g =
  case removable g of
    [] -> 0
    cs -> size cs + remove (n - 1) (foldl' (\g c -> g |. (c, (#"." □))) g cs)

g :: ".@" ▦ ℤ² = $(grid 4)

(part1, part2) :: ℤ² = (remove 1 g, remove (-1) g)
