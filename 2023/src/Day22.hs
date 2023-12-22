module Day22 (part1, part2) where

type Brick = (Int, (Coord3, Coord3))

overlapping (a', b') (c', d') =
  let (a, b) = (min a' b', max a' b')
      (c, d) = (min c' d', max c' d')
   in a <= d && b >= c

-- overlapping' ((x, y), (x', y')) ((x'', y''), (x''', y'''))
--  | x == x' && x'' == x''' && x == x'' = overlapping

intersecting b1@(_, ((x, y, z), (x', y', z'))) b2@(_, ((x'', y'', z''), (x''', y''', z'''))) =
  (x == x' && y == y' && x'' == x''' && y'' == y''' && x == x'' && y == y'' && overlapping (z, z') (z'', z''')) -- vertical interscetion
    || (z == z'' && z'' == z''' && z == z'' && overlapping (x, x') (x'', x''') && overlapping (y, y') (y'', y''')) -- hroiz intersection

restingOn b1@(i, ((x, y, z), (x', y', z'))) b2@(j, ((x'', y'', z''), (x''', y''', z'''))) =
  i /= j && min z z' == max z'' z''' + 1 && overlapping (x, x') (x'', x''') && overlapping (y, y') (y'', y''')

freefall :: [Brick] -> [Brick]
freefall =
  let fallOnce :: [Brick] -> [Brick]
      fallOnce bricks' =
        let go :: Map Int Brick -> Int -> Map Int Brick
            go bricks mi =
              let brick@(i, bc@((x, y, z), (x', y', z'))) = bricks |! mi
                  brick' = (i, both (move3 D3zN 1) bc)
               in if min z z' == 1 || (brick `restingOn`) `any` bricks
                    then -- \|| (brick' `intersecting`) `any` bricks
                      bricks
                    else bricks |. (mi, brick')
         in snd <$> unMap (foldl' go (mkMap (zip [0 ..] bricks')) [0 .. length bricks' - 1])
   in iterateFix fallOnce

canDisintegrate :: [Brick] -> [Brick]
canDisintegrate bricks =
  [ brick
    | brick <- bricks,
      let bricks' = (delete (traceShowId brick) bricks),
      freefall bricks' == bricks'
  ]

structure :: [Brick] -> (Map Brick (Set Brick), Map Brick (Set Brick))
structure bricks =
  both
    (fmap mkSet)
    ( mkMapWith (<>) [(brick, [brick']) | brick <- bricks, brick' <- bricks, brick /= brick', brick `restingOn` brick'],
      mkMapWith (<>) [(brick, [brick']) | brick <- bricks, brick' <- bricks, brick /= brick', brick' `restingOn` brick]
    )

-- 786 too high
-- 488 too high
-- 527 too high
-- not 441

canDisintegrate'' :: [Brick] -> [Brick]
canDisintegrate'' bricks =
  [ brick
    | let bricks' = freefall bricks,
      brick <- bricks',
      let (brickRestingOn, restingOnBrick) = structure (delete brick bricks'),
      -- traceShow ("removing", brick) $
      --  traceShow ("restingOnBrick", restingOnBrick) $
      --   traceShow ("brickRestingOn", brickRestingOn) $
      all (\b@(_, (_, (_, _, z))) -> z == 1 || b ∈ brickRestingOn) bricks'
  ]

canDisintegrate''' :: [Brick] -> [Brick]
canDisintegrate''' bricks =
  [ brick
    | let bricks' = freefall bricks,
      let (brickRestingOn, restingOnBrick) = structure bricks',
      brick <- bricks',
      traceShow ("removing", brick) $
        -- traceShow ("restingOnBrick", restingOnBrick) $
        -- traceShow ("brickRestingOn", brickRestingOn) $
        brick ∉ restingOnBrick
          || ( all
                 ( \b ->
                     traceShow ("testing", b, "which rests on", brickRestingOn |! b) $
                       (traceShowId $ b ∈ brickRestingOn)
                         && (traceShowId $ size (brickRestingOn |! b) > 0)
                         && (traceShowId $ brickRestingOn |! b /= mkSet [brick])
                 )
                 (restingOnBrick |! brick)
             )
  ]

canDisintegrate'''' :: [Brick] -> [Brick]
canDisintegrate'''' bricks =
  [ brick
    | let bricks' = freefall bricks,
      brick <- bricks',
      let bricks'' = traceShow brick $ delete brick bricks',
      mkSet bricks'' == mkSet (freefall bricks'')
  ]

-- 1273 bricks

part1 =
  $(input 22)
    |- (toTuple2 <$$> (toTuple3 <$$$> many1 (((number `sepBy1` char ',') `sepBy1` char '~') <* eol) <* eof))
    & zip [0 ..]
    & sortOn (thd3 . snd . snd)
    & (\bs -> traceShow (filter (\b@(_, ((_, _, z), (_, _, z'))) -> z /= z') bs) bs)
    & (\bs -> traceShow (filter (\b@(_, (ba, bb)) -> ba == bb) bs) bs)
    -- & traceShowF length
    & freefall
    -- & traceShowF length
    & sortOn (thd3 . snd . snd)
    & traceShowId
    -- & (pure >>> ([canDisintegrate, canDisintegrate'', canDisintegrate''', canDisintegrate''''] <*>))
    & (pure >>> ([canDisintegrate'''] <*>))
    & traceShowId
    & fmap length

part2 :: Text
part2 = "Part 2"
