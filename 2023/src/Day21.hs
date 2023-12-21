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
      | (n, c) ∈ seen = go q seen steps
      | otherwise =
          let seen' = ((n, c) |-> seen)
              steps' = if c == target then n |-> steps else steps
              q' =
                q
                  >< mkSeq
                    [ (n + 1, local c')
                      | g `get` c == '.',
                        c' <- neighborsNoDiags c
                    ]
           in go q' seen' steps'

walk' :: Int -> Grid Char -> IO Int
walk' n' g' = do
  seenRef <- newIORef (mkSet [] :: Set (Int, Coord2))
  nctToCsRef <- newIORef (mkMap [] :: Map (Int, Coord2, Int) [(Int, Coord2)])
  stepsToCacheRef <- newIORef (mkMap [] :: Map Coord2 (Set Int))
  let (w, h) = both (+ 1) (maxXY g')
      start = gridFindOne 'S' g'
      g = g' |. (start, '.')
      local (x, y) = (x `mod` w, y `mod` h)
      get g c = g |! local c
      go target n c@(x, y)
        | x < 0 || y < 0 || x >= w || y >= h = do
            print "teleporting"
            stepsToCache <- readIORef stepsToCacheRef
            let teleportTo = local c
            allStepsToTeleport <-
              if teleportTo ∈ stepsToCache
                then return (stepsToCache |! teleportTo)
                else do
                  let steps = traceShowId $ stepsTo g start teleportTo
                  modifyIORef stepsToCacheRef (|. (teleportTo, steps))
                  return steps
            mconcat <$> sequence [go (target - nSteps) 0 teleportTo | nSteps <- unSet allStepsToTeleport]
        | n == target = return [(n, c)]
        | otherwise = do
            print (n, target, c)
            -- seen <- readIORef seenRef
            -- if (n, c) ∈ seen
            -- if (n, c, t) ∈ seen
            --   then return []
            --   else do
            --     modifyIORef seenRef (<-| (n, c))
            --     mconcat <$> sequence [go target (n + 1) c' | c' <- neighborsNoDiags c, g `get` c' /= '#']
            nctToCs <- readIORef nctToCsRef
            case nctToCs |? (n, c, target) of
              Nothing -> do
                ncs <- mconcat <$> sequence [go target (n + 1) c' | c' <- neighborsNoDiags c, g `get` c' == '.']
                modifyIORef nctToCsRef (|. ((n, c, target), ncs))
                return ncs
              Just cs -> return cs
   in size . mkSet <$> go n' 0 start

part1 :: Int
part1 = walk 64 $(grid input 21)

-- factorize 26501365 = 5x11x481843
-- part2 = walk' 26501365 $(grid input 21)
part2 :: IO ()
part2 = do
  print =<< walk' 6 $(grid exampleInput 21)
  print =<< walk' 10 $(grid exampleInput 21)
  print =<< walk' 50 $(grid exampleInput 21)
  print =<< walk' 100 $(grid exampleInput 21)
  print =<< walk' 500 $(grid exampleInput 21)
  return ()

-- walk' 1000 $(grid exampleInput 21)
-- walk' 5000 $(grid exampleInput 21)
