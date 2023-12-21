module Day21 (part1, part2) where

walk :: Int -> Grid Char -> Int
walk n g' = size (go n (mkSet [start]))
  where
    start = gridFindOne 'S' g'
    g = g' |. (start, '.')
    go 0 cs = cs
    go n cs = go (n - 1) (mkSet [c' | c <- unSet cs, c' <- neighborsNoDiags c, g |? c' == Just '.'])

-- walk' :: Int -> Grid Char -> Int
-- walk' n' g' =
--   -- sum . fmap size . filter ((n' - 1) ∈) . fmap (setMap fst . snd) .
--   let counts = unMap $ go (mkSeq [(0, start, [start])]) (mkSet []) initCounts -- initCounts
--    in length $ nub [c | (c', ncs) <- counts, (n, c) <- unSet ncs, n == n']
--   where
--     (w, h) = both (+ 1) (maxXY g')
--     initCounts = mkMap [((x, y), mkSet []) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
--     start = gridFindOne 'S' g'
--     g = g' |. (start, '.')
--     cMod c l
--       | c == 0 = 0
--       | c < 0 = l - 1 + (c `mod` l)
--       | c < l = c
--       | otherwise = c `mod` l
--     -- local (x, y) = (x `cMod` w, y `cMod` h)
--     local (x, y) = (x `mod` w, y `mod` h)
--     get g c =
--       -- traceShow (c, local c, w, h) $
--       g |! local c
--     -- go ((n, c) :<| q) counts localCounts
--     go ((n, c@(x, y), path) :<| q) seen localCounts
--       | x < 0 || y < 0 || x >= w || y >= h = go ((n - 1, local c, local c : path) <| q)
--       | (n, c) ∈ seen = go q seen localCounts
--       -- \| local c ∈ localCounts && (n, c) ∈ (localCounts |! local c) = go q seen localCounts
--       | n > n' = localCounts
--       | otherwise =
--           -- traceShow (c, n) $
--           let localCounts' =
--                 if local c ∈ localCounts
--                   then
--                     localCounts
--                       |~ ( local c,
--                            ( ∪
--                                ( (n, c)
--                                    |-> mkSet
--                                      [ (previousN + pathOffset, pathC)
--                                        | (pathOffset, pathC) <- zip [0 .. n] path,
--                                          (previousN, previousC) <- unSet (localCounts |! local c),
--                                          pathC == previousC
--                                      ]
--                                )
--                            )
--                          )
--                   else localCounts |. (local c, mkSet [(n, c)])
--            in go
--                 (q >< mkSeq [(n - 1, c', c' : path) | c' <- neighborsNoDiags c, g `get` c' /= '#'])
--                 (seen <-| (n, c))
--                 -- (counts |~ (c, (<-| n)))
--                 localCounts'

-- need a second thing which is when do we see each thing so we can get a steps-to

stepsTo :: Grid Char -> (Int, Int) -> (Int, Int) -> Set Int
stepsTo g start target = go (mkSeq [(0, start)]) (mkSet []) (mkSet [])
  where
    (w, h) = both (+ 1) (maxXY g)
    local (x, y) = (x `mod` w, y `mod` h)
    get g c = g |! local c
    go Empty seen steps = steps
    go ((n, c) :<| q) seen steps
      | c ∈ seen = go q seen steps
      | otherwise =
          traceShow (n, c) $
            let seen' = (c |-> seen)
                steps' = if c == target then n |-> steps else steps
                q' =
                  q
                    >< mkSeq
                      [ (n + 1, local c')
                        | c' <- neighborsNoDiags c,
                          g `get` c' == '.'
                      ]
             in go q' seen' steps'

walk' :: Int -> Grid Char -> IO Int
walk' n' g' = do
  seenRef <- newIORef (mkMap [] :: Map (Int, Int, Int, Coord2) (Set (Int, Int, Coord2)))
  nctToCsRef <- newIORef (mkMap [] :: Map (Int, Coord2, Int) (Set (Int, Int, Coord2)))
  stepsToCacheRef <- newIORef (mkMap [] :: Map Coord2 (Set Int))
  let (w, h) = both (+ 1) (maxXY g')
      start = gridFindOne 'S' g'
      g = g' |. (start, '.')
      local (x, y) = (x `mod` w, y `mod` h)
      get g c = g |! local c
      go xo yo n c@(x, y)
        | n < 0 = return (mkSet [])
        | x < 0 || y < 0 || x >= w || y >= h = do
            seen <- readIORef seenRef
            if (n, xo, yo, c) ∈ seen
              then return (seen |! (n, xo, yo, c))
              else do
                let (xo', yo') =
                      if
                        | x < 0 -> (xo - 1, yo)
                        | x >= w -> (xo + 1, yo)
                        | y < 0 -> (xo, yo - 1)
                        | y >= h -> (xo, y + 1)
                        | otherwise -> error "wat"
                -- print $ "teleporting from " <> show (target, n, c) <> " to " <> show (0, target - n, local c)
                -- v <- go xo' yo' (target - n) 0 (local c)
                -- modifyIORef seenRef (|. ((xo, yo, c), v))
                -- return v
                stepsToCache <- readIORef stepsToCacheRef
                let teleportTo = local c
                v <-
                  if g |! teleportTo == '.'
                    then do
                      allStepsToTeleport <-
                        if teleportTo ∈ stepsToCache
                          then return (stepsToCache |! teleportTo)
                          else do
                            let steps = traceShowId $ stepsTo g start teleportTo
                            modifyIORef stepsToCacheRef (|. (teleportTo, steps))
                            return steps
                      mconcat <$> sequence [go xo' yo' (n - nSteps) teleportTo | nSteps <- unSet allStepsToTeleport]
                    else return (mkSet [])
                modifyIORef seenRef (|. ((n, xo, yo, c), v))
                return v
        | n == 0 = return $ mkSet [(xo, yo, c)]
        | otherwise = do
            seen <- readIORef seenRef
            if (n, xo, yo, c) ∈ seen
              then return (seen |! (n, xo, yo, c) 1)
              else do
                print (n, c, xo, yo)
                v <- foldl1 (∪) <$> sequence [go xo yo (n - 1) c' | c' <- neighborsNoDiags c, g `get` c' == '.']
                modifyIORef seenRef (|. ((n, xo, yo, c), v))
                return v
   in do
        cs <- go 0 0 n' start
        print cs
        return $ size cs

part1 :: Int
part1 = walk 64 $(grid input 21)

-- factorize 26501365 = 5x11x481843
-- part2 = walk' 26501365 $(grid input 21)
part2 :: IO ()
part2 = do
  print =<< walk' 6 $(grid exampleInput 21)
  print =<< walk' 10 $(grid exampleInput 21)
  -- print =<< walk' 50 $(grid exampleInput 21)
  -- print =<< walk' 100 $(grid exampleInput 21)
  -- print =<< walk' 500 $(grid exampleInput 21)
  return ()

-- walk' 1000 $(grid exampleInput 21)
-- walk' 5000 $(grid exampleInput 21)
