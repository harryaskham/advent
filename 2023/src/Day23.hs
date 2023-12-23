module Day23 (part1, part2) where

import Data.List.Extra (maximumOn)

slopePaths :: (Char -> [Dir2]) -> (Coord2 -> Bool) -> Coord2 -> Coord2 -> Grid Char -> [Set Coord2]
slopePaths ns stopping start end g =
  let go Empty paths = paths
      go ((c, s) :<| q) paths
        | c == end =
            -- traceShow (c, g |! c) $
            go q (end |-> s : paths)
        | c /= start && stopping c = go q paths
        | c ∈ s = go q paths
        | otherwise =
            -- traceShow (c, g |! c) $
            let q' =
                  mkSeq
                    [ (c', c |-> s)
                      | c' <- move <$> ns (g |! c) <*> pure 1 <*> pure c,
                        g |? c' ∈ (Just <$> (".<>^v" :: String))
                    ]
             in go (q >< q') paths
   in go (mkSeq [(start, (∅))]) []

convolve :: (GridCell c, GridCell d) => (Int, Int, Int, Int) -> (Grid c -> d) -> Grid c -> Grid d
convolve (u, d, l, r) f g =
  mkGrid
    [ (c, f g')
      | c@(x', y') <- coords g,
        let g' = mapCoords (\(x, y) -> (x - x', y - y')) $ filterCoords (\(x, y) -> x >= x' - l && y >= y' - u && x <= x' + r && y <= y' + d) g
    ]

findForks :: Grid Char -> Set Coord2
findForks =
  mkSet
    . gridFind True
    . convolve
      (1, 1, 1, 1)
      (\g -> g |? (0, 0) == Just '.' && length (filter (∈ (Just <$> (".<>^v" :: String))) ((g |?) <$> neighborsNoDiags (0, 0))) > 2)

allPaths :: Grid Char -> Set Coord2 -> Map Coord2 (Map Coord2 [Set Coord2])
allPaths g forks =
  fmap (mkMapWith (<>)) . mkMapWith (<>) . mconcat $
    [ [(start, [(end, [path])]), (end, [(start, [path])])]
      | (start, end) <- triPairs (unSet forks),
        path <- traceShow (start, end) $ slopePaths (const enumerate) (∈ forks) start end g
    ]

longestPathGraph :: Map Coord2 (Map Coord2 [Set Coord2]) -> Coord2 -> Coord2 -> Set Coord2 -> Maybe (Set Coord2)
longestPathGraph graph start end allSeen =
  -- let -- go :: Seq (Coord2, Set Coord2, Map Coord2 (Map Coord2 [Set Coord2]), [Coord2]) -> Set [Coord2] -> Set [Coord2] -> [Coord2]
  let go Empty paths _ =
        traceShow ("exhausted q", size <$> paths) $
          paths
      go ((c, s, graph, nodePath) :<| q) paths seen
        | c == end =
            traceShow (c, "found end with len", size s) $
              go q (s : paths) seen
        | nodePath ∈ (seen :: Set [Coord2]) =
            traceShow ("already seen", nodePath) $
              go q paths seen
        | otherwise =
            traceShow (start, end, c, nodePath, size s, size q, take 1 $ sortOn Down (size <$> paths)) $
              let q' =
                    mkSeq
                      [ (c', s', graph', nodePath')
                        | (c', paths) <- maybe [] unMap (graph |? c),
                          path <- paths,
                          s ∩ path == mkSet [c],
                          let s' = s ∪ path,
                          let graph' = filter (\p -> p ∩ s' ∈ [(∅), mkSet [c']]) <$$> graph,
                          let nodePath' = c' : nodePath
                      ]
               in -- in go (q >< q') paths (nodePath |-> seen)
                  catMaybes [longestPathGraph graph c' end s' | (c', s', _, _) <- unSeq q']
   in case go (mkSeq [(start, start |-> allSeen, graph, [start])]) [] (∅) of
        [] -> Nothing
        ps -> Just (maximumOn size ps)

intersectingEdges graph =
  [ (a, b)
    | (a, toA) <- unMap graph,
      (b, abPaths) <- unMap toA,
      (c, acPaths) <- unMap toA,
      abPath <- abPaths,
      acPath <- acPaths,
      b /= c,
      size (abPath ∩ acPath) > 1
  ]

-- edge list of (a, b, path, [intersects with])
-- lazily output all paths, append as we go, check for dupes
-- not that big a graph, filter all paths
-- only one path between nodes! use this - not true

longestPathGraph' :: Map Coord2 (Map Coord2 [Set Coord2]) -> Coord2 -> Coord2 -> [Int]
longestPathGraph' graph start end = startEvalMemo $ go (start, (∅))
  where
    go (c, s)
      | c == end = traceShow "end" $ return [0]
      -- \| c ∈ p = return [-1000000000000000]
      | otherwise =
          -- traceShow (c, size s) $
          mconcat
            <$> sequence
              [ (size path +) <$$> memo go (c', s')
                | (c', paths) <- maybe [] unMap (graph |? c),
                  path <- paths,
                  let s' = s ∪ path,
                  -- let p' = c |-> p
                  (s ∩ path) ∈ [(∅), mkSet [c]]
              ]

-- build a structure which is:
-- a to b, pth length, and which edges it invalidates
-- can then build small graphw

edgeInfo graph =
  mkMapWith (<>) $
    [ ((a,b),[(bPath,
               size bPath,
               mkSet . fmap thd3 $
                 filter
                   (\(a',b',p)->(a,b)/=(a',b') && ((a==a' || b==b' || let i = bPath ∩ p in (a'==b && i /= mkSet [b]) || i /=  (∅))))
                   allPaths)])-- (p ∈ [(∅), mkSet [b]])) allPaths)])
      | (a, aTo) <- unMap graph,
      (b, bPaths) <- unMap aTo,
      bPath <- bPaths
    ]
  where
    allPaths = [(a,b,p) | (a, paths) <- unMap graph, (b,ps) <- unMap paths, p <- ps]

maxi [] = Nothing
maxi xs = Just (maximum xs)

lazyPaths :: Map Coord2 (Map Coord2 [Set Coord2]) -> Coord2 -> Coord2 -> Maybe Int
lazyPaths graph start end =  startEvalMemo $ go (start, (∅), (∅))
  where
    e = traceShowF keys $ edgeInfo graph
    go (c, es, invalid)
      | c == end = return $ Just 0
      | otherwise =
        traceShow ("at",c, es,invalid) $ do
          -- todo take maximum
          -- track taken and invalid
          lens <- sequence
                    [ traceShow ("going to",a,b,l,invalid',es) $
                        (l+) <$$> memo go (b, e |->es, invalid'  ∪ invalid)
                    | ((a,b),ls) <- unMap e,
                      (e,l,invalid') <- ls,
                      a == c || b == c,
                      traceShow ("testing",a,b,l,e,es) $ True,
                      traceShowId $ e ∉ es,
                      traceShowId $ e ∉ invalid,
                      (traceShowId $ traceShow "crosses any of our edges?" (es ∩ invalid')) == (∅)
                      -- e ∉ (traceShow ("testing",a,b,l,e,es) $ traceShowId invalid)
                      -- es ∩ (traceShow ("testing",a,b,l,e,es) $ traceShowId invalid') == (∅)
                      --(c', paths) <- maybe [] unMap (graph |? c),
                      --path <- paths,
                      --let s' = s ∪ path \\ mkSet [c'],
                      ---- let p' = c |-> p
                      --(s ∩ path) ∈ [(∅), mkSet [c]]
                    ]
          return . maxi $ catMaybes lens

longest :: Map Coord2 (Map Coord2 [Set Coord2]) -> Coord2 -> Coord2 -> Set Coord2 -> Maybe (Set Coord2)
longest graph start end seen =
  traceShow "starting longest" $
    startEvalMemo $
      longest' (start, seen)
  where
    longest' (c, seen)
      | c == end = traceShow "found end" $ return . Just $ c |-> seen
      | otherwise =
          -- traceShow (c, size seen) $
          do
            longestPaths <-
              forM
                (maybe [] unMap (graph |? c))
                ( \(c', paths) ->
                    forM
                      paths
                      ( \path -> do
                          if seen ∩ path == mkSet [c]
                            then memo longest' (c', seen ∪ path)
                            else return Nothing
                      )
                )
            case catMaybes $ mconcat longestPaths of
              [] -> return Nothing
              paths -> return . Just $ maximumOn size paths

parts =
  -- let g = $(grid input 23)
  let g = $(grid exampleInput 23)
      (maxX, maxY) = maxXY g
      (start, end) = ((1, 0), (maxX - 1, maxY))
      paths = slopePaths (\c -> bool [fromArrow2 c] enumerate (c == '.')) (const False) start end g
      forks = findForks g
      graph = allPaths g (mkSet [start, end] ∪ forks)
      -- path' = longestPathGraph graph start end (∅)
      -- path' = longestPathGraph' graph start end
      -- paths' = longestPathGraph' graph start end
      paths' = lazyPaths graph start end
   in -- paths' = case path' of
      --  Nothing -> []
      --  Just p -> [p]
      -- path' = fromMaybe (∅) (longest graph start end (∅))
      -- paths' = [path']
      (subtract 1 . maximum . fmap length $ paths, paths')

part1 :: Int
part1 = fst parts

part2 = snd parts
