module Day22 (part1, part2) where

type Brick = (Coord3, Coord3)

overlapping :: (Ord a) => (a, a) -> (a, a) -> Bool
overlapping (a', b') (c', d') =
  let (a, b) = (min a' b', max a' b')
      (c, d) = (min c' d', max c' d')
   in a <= d && b >= c

restingOn :: Brick -> Brick -> Bool
restingOn ((x, y, z), (x', y', z')) ((x'', y'', z''), (x''', y''', z''')) =
  min z z' == max z'' z''' + 1
    && overlapping (x, x') (x'', x''')
    && overlapping (y, y') (y'', y''')

onFloor :: Brick -> Bool
onFloor ((_, _, z), (_, _, z')) = min z z' == 1

freefall :: Vector Brick -> Vector Brick
freefall bricks =
  let go bricks i =
        let brick = bricks !! i
         in if onFloor brick || (brick `restingOn`) `any` bricks
              then bricks
              else bricks !. (i, both (move3 D3zN 1) brick)
      fallOnce bricks' = foldl' go bricks' [0 .. length bricks' - 1]
   in iterateFix fallOnce bricks

structure :: Vector Brick -> (Map Brick (Set Brick), Map Brick (Set Brick))
structure bricks =
  let m :: (Brick -> Brick -> Bool) -> Map Brick (Set Brick)
      m f =
        mkWith
          (∪)
          ( [ (brick, co [brick'])
              | brick <- bricks,
                brick' <- bricks,
                brick /= brick',
                brick `f` brick'
            ]
          )
   in both m (restingOn, flip restingOn)

disintegrateOne :: Vector Brick -> ℤ'
disintegrateOne bricks =
  let (brickRestingOn, restingOnBrick) = structure bricks
   in size
        [ brick
          | brick <- bricks,
            brick ∉ restingOnBrick
              || all
                ((&&) <$> (∈ brickRestingOn) <*> ((brickRestingOn |!) >>> (/= co [brick])))
                (restingOnBrick |! brick)
        ]

disintegrateAll :: Vector Brick -> ℤ'
disintegrateAll bricks =
  let brickRestingOn = fst $ structure bricks
   in sum
        [ subtract 1 . size . fst $
            iterateFix
              ( \(fallen, bricks) ->
                  let fallen' = setFilter (\brick -> (λ ¬ onFloor brick) && brickRestingOn |! brick ∖ fallen == (∅)) bricks
                   in (fallen ∪ fallen', bricks ∖ fallen')
              )
              (co ([brick], bricks))
          | brick <- bricks
        ]

fallen :: Vector Brick
fallen =
  $(input 22)
    |- (toTuple2 <$$> (toTuple3 <$$$> many1 (((number `sepBy1` char ',') `sepBy1` char '~') <* eol) <* eof))
    & sortOn (thd3 . snd)
    & mkVec
    & freefall

part1 :: ℤ'
part1 = disintegrateOne fallen

part2 :: ℤ'
part2 = disintegrateAll fallen
