module Day4 (part1, part2) where

removable :: ".@" ▦ ℤ² -> [ℤ²]
removable g = [c | c <- coords g, g |! c == (#"@" □), size [n | n <- neighs @8 @[] c g, g |! n == (#"@" □)] < 4]

remove :: ℤ -> ".@" ▦ ℤ² -> ℤ
remove 0 _ = 0
remove n g =
  case removable g of
    [] -> 0
    cs -> size cs + remove (n - 1) (foldl' (\g c -> g |. (c, (#"." □))) g cs)

g :: ".@" ▦ ℤ² = $(grid 4)

(part1, part2) :: ℤ² = (remove 1 g, remove (-1) g)
