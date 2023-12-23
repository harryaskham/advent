module Day23 (part1, part2) where

slopePaths :: (Char -> [Dir2]) -> (Coord2 -> Bool) -> Coord2 -> Coord2 -> Grid Char -> [Set Coord2]
slopePaths ns stopping start end g =
  let go Empty paths = paths
      go ((c, s) :<| q) paths
        | c == end = go q (end |-> s : paths)
        | c /= start && stopping c = go q paths
        | c ∈ s = go q paths
        | otherwise =
            let q' =
                  mkSeq
                    [ (c', c |-> s)
                      | c' <- move <$> ns (g |! c) <*> pure 1 <*> pure c,
                        g |? c' ∈ (Just <$> (".<>^v" :: String))
                    ]
             in go (q >< q') paths
   in go (mkSeq [(start, (∅))]) []

findForks :: Grid Char -> Set Coord2
findForks =
  mkSet
    . gridFind True
    . convolve
      (1, 1, 1, 1)
      ( \g ->
          g |? (0, 0) == Just '.'
            && length
              ( filter
                  (∈ (Just <$> (".<>^v" :: String)))
                  ((g |?) <$> neighborsNoDiags (0, 0))
              )
              > 2
      )

mkGraph :: Grid Char -> Set Coord2 -> Map Coord2 (Map Coord2 [Set Coord2])
mkGraph g forks =
  fmap (mkMapWith (<>)) . mkMapWith (<>) . mconcat $
    [ [(start, [(end, [path])]), (end, [(start, [path])])]
      | (start, end) <- triPairs (unSet forks),
        path <- slopePaths (const enumerate) (∈ forks) start end g
    ]

allPaths :: Map Coord2 (Map Coord2 [Set Coord2]) -> Coord2 -> Coord2 -> Int
allPaths graph start end = fromMaybe (-1) $ go start (∅)
  where
    go a seen
      | a == end = Just 0
      | a ∈ seen = Nothing
      | otherwise =
          let ls =
                [ (-1 + size p +) <$> go b (a |-> seen)
                  | (b, ps) <- unMap (graph |! a),
                    b ∉ seen,
                    p <- ps
                ]
           in case catMaybes ls of
                [] -> Nothing
                ls -> Just (maximum ls)

parts :: (Int, Int)
parts =
  let g = $(grid input 23)
      (maxX, maxY) = maxXY g
      (start, end) = ((1, 0), (maxX - 1, maxY))
      paths = slopePaths (\c -> bool [fromArrow2 c] enumerate (c == '.')) (const False) start end g
      forks = findForks g
      graph = mkGraph g (mkSet [start, end] ∪ forks)
   in (subtract 1 . maximum . fmap length $ paths, allPaths graph start end)

part1 :: Int
part1 = fst parts

part2 :: Int
part2 = snd parts
