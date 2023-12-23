module Day23 (part1, part2) where

longestPath :: Grid Char -> Int
longestPath g =
  let (maxX, maxY) = maxXY g
      ns '.' = enumerate
      ns c = [fromArrow2 c]
      go Empty = []
      go ((c, n, s) :<| q)
        | c == (maxX - 1, maxY) = n : go q
        | c ∈ s = go q
        | otherwise = go (q >< mkSeq [(c', n + 1, c |-> s) | c' <- move <$> ns (g |! c) <*> pure 1 <*> pure c, g |? c' ∈ (Just <$> (".<>^v" :: String))])
   in maximum $ go (mkSeq [((1, 0), 0, mkSet [])])

part1 :: Int
part1 = longestPath $(grid input 23)

part2 :: Text
part2 = "Part 2"
