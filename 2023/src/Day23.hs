module Day23 (part1, part2) where

pathsForks :: (Char -> [Dir2]) -> Coord2 -> Coord2 -> Grid Char -> (Set (Set Coord2), Set Coord2)
pathsForks ns start target g =
  let go Empty paths forks = (paths, forks)
      go ((c, s) :<| q) paths forks
        | c == target = go q (target |-> s |-> paths) forks
        | c ∈ s = go q paths forks
        | otherwise =
            let q' =
                  mkSeq
                    [ (c', c |-> s)
                      | c' <- move <$> ns (g |! c) <*> pure 1 <*> pure c,
                        g |? c' ∈ (Just <$> (".<>^v" :: String))
                    ]
                forks' = if size q' > 1 then c |-> forks else forks
             in go (q >< q') paths forks'
   in go (mkSeq [(start, (∅))]) (∅) (∅)

allPaths :: Grid Char -> Set Coord2 -> Map Coord2 (Map Coord2 (Set (Set Coord2)))
allPaths g forks =
  let paths start end = undefined
   in ( mkSet
          <$$> ( mkSet
                   <$$$> ( mkMapWith (<>)
                             <$> mkMapWith
                               (<>)
                               ( mconcat
                                   [ [(start, [(end, [path])]), (end, [(start, [path])])]
                                     | (start, end) <- triPairs (unSet forks),
                                       path <- paths start end
                                   ]
                               )
                         )
               )
      )

longestPath :: Map Coord2 (Map Coord2 (Set (Set Coord2))) -> Int
longestPath graph = undefined

parts :: (Int, Int)
parts =
  let g = $(grid input 23)
      (maxX, maxY) = maxXY g
      (paths, forks) = pathsForks (\c -> bool [fromArrow2 c] enumerate (c == '.')) (1, 0) (maxX - 1, maxY) g
      graph = allPaths g forks
   in ( maximum (length <$> unSet paths),
        longestPath graph
      )

part1 :: Int
part1 = fst parts

part2 :: Int
part2 =
  let g = $(grid input 23)
      (maxX, maxY) = maxXY g
      (paths, forks) = pathsForks (const enumerate) (1, 0) (maxX - 1, maxY) g
      graph = allPaths g forks
   in maximum (length <$> unSet paths)