module Day16 (part1, part2) where

-- [grid|#.ES|]
data C = Wall | Space | Start | End deriving (Show, Eq, Ord)

instance GridCell C where
  cs = [(Wall, '#'), (Space, '.'), (Start, 'S'), (End, 'E')]

graph :: G ℤ² C -> Map ℤ² (Map ℤ² (Dir², Set ℤ²))
graph g = go (mk₁ (g |!> Start)) ø
  where
    nds c = [c `goingTo` n | n <- vicinity @4 c, g |! n ≢ Wall]
    straight path c d
      | size path > 0 ∧ isNode c = (c, path)
      | otherwise =
          let c' = move @ℤ d 1 c
           in case g |! c' of
                Wall -> (c, path)
                _ -> straight (c |-> path) c' d
    isNode c =
      case nds c of
        [[d], [d']] -> d ≢ opposite @Dir² d'
        _ -> True
    go Empty gr = gr
    go (c :<| q) gr
      | c ∈ gr = go q gr
      | otherwise =
          let nodes = [(c', (d, line)) | d <- (enumerate @Dir²), let (c', line) = straight ø c d, c ≢ c']
           in go (q >< mk (fst <$> nodes)) (gr |. (c, mkMap nodes))

paths :: ℤ² -> ℤ² -> Map ℤ² (Map ℤ² (Dir², Set ℤ²)) -> (ℤ, ℤ)
paths start end gr = go (round (∞)) (mkQ₁ h (start, DirRight, 0, ø, ø)) [] ø
  where
    h (c, facing, n, _, seen) =
      sum
        [ n,
          manhattan c end,
          1000 ⋅ maximum (0 : [turnDiff d facing | d <- c `goingTo` end])
        ]
    go cap ((est, (c, facing, n, path, seen)) :<! q) paths (gseen :: Map (ℤ², Dir²) ℤ)
      | n > cap = (cap, size (foldl1 (∪) paths))
      | c ≡ end = go n q ((end |-> path) : paths) (gseen |. ((c, facing), n))
      | (c, facing) ∈ seen = go cap q paths gseen
      | (c, facing) ∈ gseen ∧ gseen |! (c, facing) ≢ n = go cap q paths gseen
      | otherwise =
          go
            cap
            ( foldl'
                ( \q (c', (facing', path')) ->
                    qInsert
                      h
                      ( c',
                        facing',
                        n + size path' + 1000 ⋅ (turnDiff facing facing'),
                        path' ∪ path,
                        (c, facing) |-> seen
                      )
                      q
                )
                q
                (unMap (gr |! c))
            )
            paths
            (gseen |. ((c, facing), n))

(part1, part2) :: (ℤ, ℤ) =
  let (g :: G ℤ² C) = $(grid 16)
      gr = graph g
   in paths (g |!> Start) (g |!> End) gr
