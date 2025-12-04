module Day4 (part1, part2) where

remove :: ℤ -> ".@" ▦ ℤ² -> ℤ
remove 0 _ = 0
remove n g =
  let cs = [c | c <- coords g, g |! c == (#"@" □), size [n | n <- neighs @8 @[] c g, g |! n == (#"@" □)] < 4]
   in case cs of
        [] -> 0
        cs -> size cs + remove (n - 1) (foldl' (\g c -> g |. (c, (#"." □))) g cs)

(part1, part2) :: ℤ² =
  let (g :: ".@" ▦ ℤ²) = $(grid 4)
   in ( remove 1 g,
        remove (-1) g
      )
