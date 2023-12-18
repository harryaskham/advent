module Day18 (part1, part2) where

import Data.List.Extra (groupBy)
import Data.Tuple.Extra ()
import Data.Tuple.Utils (fst3)

tracePerimeter' :: [(Dir2, Int)] -> Set (Coord2, Int)
tracePerimeter' =
  let go _ _ seen [] = seen
      go i c seen ((_, 0) : rest) = go i c seen rest
      go i c seen ((d, n) : rest) = go (i + 1) (move d 1 c) ((c, i) |-> seen) ((d, n - 1) : rest)
   in go 0 (0, 0) (mkSet [])

-- y edge start and end, keyed and sorted by there ex coordinate
verticals' :: [(Dir2, Int)] -> [(Int, [(Int, Int)])]
verticals' ms =
  let go _ [_] _ = []
      go c@(x, y) ((d, n) : rest@((nextD, _) : _)) lastD =
        case (d, lastD, nextD) of
          (DirUp, DirLeft, DirLeft) -> (x, (y - n + 1, y)) : go (move d n c) rest d
          (DirUp, DirLeft, DirRight) -> (x, (y - n, y)) : go (move d n c) rest d
          (DirUp, DirRight, DirLeft) -> (x, (y - n + 1, y - 1)) : go (move d n c) rest d
          (DirUp, DirRight, DirRight) -> (x, (y - n, y - 1)) : go (move d n c) rest d
          (DirDown, DirLeft, DirLeft) -> (x, (y, y + n - 1)) : go (move d n c) rest d
          (DirDown, DirLeft, DirRight) -> (x, (y, y + n)) : go (move d n c) rest d
          (DirDown, DirRight, DirLeft) -> (x, (y + 1, y + n - 1)) : go (move d n c) rest d
          (DirDown, DirRight, DirRight) -> (x, (y + 1, y + n)) : go (move d n c) rest d
          _ -> go (move d n c) rest d
   in sort (unMap (mkMapWith (<>) (second pure <$> sort (go (0, 0) (ms <> take 1 ms) (fst (ulast ms))))))

-- y edge start and end, keyed and sorted by there ex coordinate
verticals :: [(Dir2, Int)] -> [(Int, [(Int, Int)])]
verticals ms =
  let go _ [_] _ = []
      go c@(x, y) ((d, n) : rest@((nextD, _) : _)) lastD =
        case (d, lastD, nextD) of
          (DirUp, DirLeft, DirLeft) -> (x, (y - n, y)) : go (move d n c) rest d
          (DirUp, DirLeft, DirRight) -> (x, (y - n, y)) : go (move d n c) rest d
          (DirUp, DirRight, DirLeft) -> (x, (y - n, y)) : go (move d n c) rest d
          (DirUp, DirRight, DirRight) -> (x, (y - n, y)) : go (move d n c) rest d
          (DirDown, DirLeft, DirLeft) -> (x, (y, y + n)) : go (move d n c) rest d
          (DirDown, DirLeft, DirRight) -> (x, (y, y + n)) : go (move d n c) rest d
          (DirDown, DirRight, DirLeft) -> (x, (y, y + n)) : go (move d n c) rest d
          (DirDown, DirRight, DirRight) -> (x, (y, y + n)) : go (move d n c) rest d
          _ -> go (move d n c) rest d
   in sort (unMap (mkMapWith (<>) (second pure <$> sort (go (0, 0) (ms <> take 1 ms) (fst (ulast ms))))))

scanFromLeft :: [(Int, [(Int, Int)])] -> Int
-- scanFromLeft (v : vs) = go (sumActive (snd v)) v vs
scanFromLeft (v : vs) = go (sumActive (snd v)) (snd v) (v : vs)
  where
    -- go n oldActive [(x, edges), (x', edges')] = (n + (x' - x) * sumActive oldActive)
    go n oldActive [_] = n
    go n oldActive ((x, edges) : (x', edges') : rest) =
      let active = splitActive oldActive edges'
       in traceShow (n, "|", x, x', oldActive, active) $
            -- getting broken by last edges
            go (n + (x' - x - 1) * sumActive oldActive + sumActive (mergeSegs oldActive edges')) active ((x', edges') : rest)
    mergeSegs active edges =
      -- traceShow (active, edges) . traceShowId $
      unSet $
        iterateFix mergeAll (mkSet (active <> edges))
    mergeOne (a, b) (c, d)
      | overlapping (a, b) (c, d) = [(min a c, max b d)]
      | otherwise = [(a, b), (c, d)]
    mergeAll s = mkSet $ concat [mergeOne a b | a <- unSet s, b <- unSet s]
    -- go n (x, active) [(x', edges)] =
    --   traceShow (x, n, active) $
    --     n + (x' - x) * sumActive active
    -- traceShow (x, n, snd $ ulast vs) $
    --   n + sumActive (snd $ ulast vs)
    -- go n (x, active) ((x', edges) : vs) =
    --   let active' = splitActive active edges
    --       oldSum = sumActive active
    --       newSum = sumActive active'
    --    in traceShow (x, n, active) $
    --         go (n + (x' - x - 1) * sumActive active + (max oldSum newSum)) (x', active') vs
    sumActive active = sum $ (\(a, b) -> b - a + 1) <$> active
    splitActive active edges =
      (mconcat [overlaps a edges | a <- active])
        <> [e | e <- edges, not (any (overlapping e) active)]
        <> [a | a <- active, not (any (overlapping a) edges)]
    overlapping (a, b) (c, d) = c <= b && d >= a
    overlaps (a, b) edges = mconcat [cut (a, b) (c, d) | (c, d) <- edges, overlapping (a, b) (c, d)]
    cut (a, b) (c, d)
      | (a, b) == (c, d) = [(a, c), (b, d)] -- only keep the two thin lines
      | c <= a && d >= b = [] -- completely subsumed
      | c < a && d <= b = [(d, b)] -- only let through the active bit;should we let through any of the edge?
      | c >= a && d <= b = [(a, c), (d, b)] -- age subsumed, so it gets chocked out
      | c >= a && d > b = [(a, c)] -- again only let the active bit through

-- just make the vertical segments their full height and only intersect one in from the top and bottom

tracePerimeter :: [(Dir2, Int)] -> Set (Coord2, Int)
tracePerimeter =
  let go _ _ seen [] = seen
      go i (x, y) seen ((d, n) : rest) =
        traceShow i $
          let cs = case d of
                DirDown -> [((x, y + j), i + j - 1) | j <- [1 .. n]]
                DirUp -> [((x, y - j), i + j - 1) | j <- [1 .. n]]
                DirLeft -> [((x - j, y), i + j - 1) | j <- [1 .. n]]
                DirRight -> [((x + j, y), i + j - 1) | j <- [1 .. n]]
           in go (i + n) (fst $ ulast cs) (seen ∪ mkSet cs) rest
   in go 0 (0, 0) (mkSet [])

floodFill :: Set Coord2 -> Set Coord2
floodFill p =
  let go qs =
        let f (Empty, seen) = Right seen
            f (c :<| q, seen)
              | c ∈ seen = Left (q, seen)
              | otherwise = Left (q >< mkSeq (neighborsNoDiags c), c |-> seen)
         in case partitionEithers (f <$> qs) of
              (_, seen : _) -> seen
              (qs', _) -> go qs'
   in go ((,p) <$> (mkSeq . pure <$> [(-1, -1), (-1, 1), (1, -1), (1, 1)]))

volume :: Set (Coord2, Int) -> Int
volume p =
  let maxI = maximum $ snd <$> unSet p
      switches (((x, y), i), ((x', _), i')) =
        -- let s = traceShowId $ nub [y' - y | y' <- [y - 1, y + 1], j <- [i + 1, i - 1], ((x, y'), j) ∈ p || (i == 0 && ((x, y'), maxI) ∈ p) || (i == maxI && ((x, y'), 0 :: Int) ∈ p)]
        --     s' = traceShowId $ nub [y' - y | y' <- [y - 1, y + 1], j <- [i' + 1, i' - 1], ((x', y'), j) ∈ p || (i' == 0 && ((x', y'), maxI) ∈ p) || (i' == maxI && ((x', y'), 0 :: Int) ∈ p)]
        let s = nub [y' - y | y' <- [y - 1, y + 1], j <- [i + 1, i - 1], ((x, y'), j) ∈ p || (i == 0 && ((x, y'), maxI) ∈ p) || (i == maxI && ((x, y'), 0 :: Int) ∈ p)]
            s' = nub [y' - y | y' <- [y - 1, y + 1], j <- [i' + 1, i' - 1], ((x', y'), j) ∈ p || (i' == 0 && ((x', y'), maxI) ∈ p) || (i' == maxI && ((x', y'), 0 :: Int) ∈ p)]
         in length s == 2 || length s == 1 && length s' == 1 && s /= s'
      merge [] [] = []
      merge [] s = [(ulast s, uhead s)]
      merge (x : xs) [] = merge xs [x]
      merge (x@((c, _), i) : xs) s@(x'@((c', _), i') : xs')
        | c - c' == 1 && abs (i - i') == 1 = merge xs (x : x' : xs')
        | otherwise = (ulast s, uhead s) : merge xs [x]
      merge' xs = merge xs []
      (minY, maxY) = (minimum $ snd . fst <$> unSet p, maximum $ snd . fst <$> unSet p)
      unmergedRows :: [[(Coord2, Int)]]
      unmergedRows =
        snd
          <$> ( sort $
                  unMap
                    ( sort
                        <$> foldl'
                          ( \rows ((x, y), i) ->
                              traceShow (i, maxI) $
                                rows |~ (y, (((x, y), i) :))
                          )
                          (mkMap [(y, []) | y <- unSet (setMap (snd . fst) p)])
                          (unSet p)
                    )
              )
      mergedRows :: [[((Coord2, Int), (Coord2, Int))]]
      mergedRows = [traceShow (y, length unmergedRows) $ merge' row | (y, row) <- zip [0 ..] unmergedRows]
      rows' :: [[(Int, Int, Bool)]]
      rows' = (\s@(((x, y), _), ((x', _), _)) -> traceShow (y, maxY) (x, x', switches s)) <$$> mergedRows
      deduplicatedRows = unMap $ countMap rows'
      rows =
        -- traceShow " "
        --   . traceShowId
        --   . merge'
        merge'
          -- . traceShowId
          . snd
          <$> (sort $ unMap (sort <$> foldl' (\rows ((x, y), i) -> rows |~ (y, (((x, y), i) :))) (mkMap [(y, []) | y <- unSet (setMap (snd . fst) p)]) (unSet p)))
      -- rows = merge' . snd <$> (sort $ unMap (sort <$> foldl' (\rows (x, y) -> rows |~ (y, (x :))) (mkMap [(y, []) | y <- unSet (setMap snd p)]) (unSet p)))
      rowVolume :: [((Coord2, Int), (Coord2, Int))] -> Int
      rowVolume row =
        -- traceShowId $
        fst3 $
          foldl'
            ( \(v, last, inside) s@(((a, y), _), ((b, _), _)) -> case (last, inside) of
                (Nothing, _) ->
                  -- traceShow (s, v, last, inside, switches s, "not taking gap") $
                  ( v + (b - a + 1),
                    if switches s then Just (b + 1) else Nothing,
                    if switches s then not inside else inside
                  )
                (Just b', _) ->
                  -- traceShow (s, v, last, inside, switches s, "taking segment and gap") $
                  ( v + (a - b') + (b - a + 1),
                    if switches s then Nothing else Just (b + 1),
                    not (switches s)
                  )
                  -- (Just b',False) -> (v + (a - b') + (b - a + 1), if switches s then Nothing else Just (b +1),not (switches s))
                  -- if y ∈ [minY, maxY]
                  --   then (v + (b - a + 1), Nothing)
                  --   else (v + (a - b') + (b - a + 1), Nothing)
            )
            (0, Nothing, False)
            row
      rowVolume' :: [(Int, Int, Bool)] -> Int
      rowVolume' row =
        -- traceShowId $
        fst $
          foldl'
            ( \(v, last) (a, b, switch) -> case last of
                Nothing ->
                  -- traceShow (s, v, last, inside, switches s, "not taking gap") $
                  ( v + (b - a + 1),
                    if switch then Just (b + 1) else Nothing
                  )
                Just b' ->
                  -- traceShow (s, v, last, inside, switches s, "taking segment and gap") $
                  ( v + (a - b') + (b - a + 1),
                    if switch then Nothing else Just (b + 1)
                  )
                  -- (Just b',False) -> (v + (a - b') + (b - a + 1), if switches s then Nothing else Just (b +1),not (switches s))
                  -- if y ∈ [minY, maxY]
                  --   then (v + (b - a + 1), Nothing)
                  --   else (v + (a - b') + (b - a + 1), Nothing)
            )
            (0, Nothing)
            row
   in --  in sum (rowVolume <$> rows)
      sum (uncurry (*) . first rowVolume' <$> deduplicatedRows)

solve :: (((Dir2, Int), (Dir2, Int)) -> (Dir2, Int)) -> Int
solve f =
  $(exampleInput 18)
    |- ( f
           <$$> ( many
                    ( (,)
                        <$> ((,) <$> (udlrToDir2 <$> anyChar <* spaces) <*> (number <* spaces))
                        <*> ( swap
                                <$> ( string "(#"
                                        *> ( (,)
                                               <$> (uread . ("0x" <>) <$> count 5 hexDigit)
                                               <*> (([DirRight, DirDown, DirLeft, DirUp] !!) <$> number)
                                           )
                                        <* (char ')' >> eol)
                                    )
                            )
                    )
                    <* eof
                )
       )
    & verticals
    & scanFromLeft

-- & tracePerimeter
-- & floodFill
-- & size
-- & volume

part1 :: Int
part1 = solve fst

part2 :: Int
part2 = solve snd
