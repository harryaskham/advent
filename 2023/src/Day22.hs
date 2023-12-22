module Day22 (part1, part2) where

type Brick = (Int, (Coord3, Coord3))

overlapping (a', b') (c', d') =
  let (a, b) = (min a' b', max a' b')
      (c, d) = (min c' d', max c' d')
   in a <= d && b >= c

-- overlapping' ((x, y), (x', y')) ((x'', y''), (x''', y'''))
--  | x == x' && x'' == x''' && x == x'' = overlapping

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

structure :: [Brick] -> (Map Brick (Set Brick), Map Brick (Set Brick))
structure bricks =
  both
    (fmap mkSet)
    ( mkMapWith (<>) [(brick, [brick']) | brick <- bricks, brick' <- bricks, brick /= brick', brick `restingOn` brick'],
      mkMapWith (<>) [(brick, [brick']) | brick <- bricks, brick' <- bricks, brick /= brick', brick' `restingOn` brick]
    )

disintegrateOne :: [Brick] -> [Brick]
disintegrateOne bricks =
  [ brick
    | let bricks' = bricks,
      let (brickRestingOn, restingOnBrick) = structure bricks',
      brick <- bricks',
      brick ∉ restingOnBrick
        || all
          ( \b ->
              (b ∈ brickRestingOn)
                && (size (brickRestingOn |! b) > 0)
                && (brickRestingOn |! b /= mkSet [brick])
          )
          (restingOnBrick |! brick)
  ]

disintegrateAll :: [Brick] -> Int
disintegrateAll bricks =
  let (brickRestingOn, restingOnBrick) = structure bricks
      go fallen Empty = traceShowF size fallen
      go fallen (brick :<| q)
        | brick ∉ restingOnBrick = go (brick |-> fallen) q
        | otherwise = go (brick |-> fallen) (q >< mkSeq [b | b <- unSet (restingOnBrick |! brick), ((brickRestingOn |! b \\\ fallen) == (mkSet []) || (brickRestingOn |! b \\\ fallen) == (mkSet [b]))])
   in size $ go (mkSet []) . pure <$> (sortOn (thd3 . snd . snd) bricks)

part1 :: Int
part1 =
  $(input 22)
    |- (toTuple2 <$$> (toTuple3 <$$$> many1 (((number `sepBy1` char ',') `sepBy1` char '~') <* eol) <* eof))
    & zip [0 ..]
    & sortOn (thd3 . snd . snd)
    & freefall
    & disintegrateOne
    & length

-- 1273 too low

part2 :: Int
part2 =
  $(input 22)
    |- (toTuple2 <$$> (toTuple3 <$$$> many1 (((number `sepBy1` char ',') `sepBy1` char '~') <* eol) <* eof))
    & zip [0 ..]
    & sortOn (thd3 . snd . snd)
    & freefall
    & disintegrateAll
