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

freefall :: [Brick] -> [Brick]
freefall bricks =
  let go bricks i =
        let brick = bricks !! i
         in if onFloor brick || (brick `restingOn`) `any` bricks
              then bricks
              else bricks !. (i, both (move3 D3zN 1) brick)
      fallOnce bricks' =  foldl' go bricks' [0 .. length bricks' - 1]
   in unVec $ iterateFix fallOnce (mkVec bricks)

structure :: [Brick] -> (Map Brick (Set Brick), Map Brick (Set Brick))
structure bricks =
  let mk f =
        mkSet
          <$> mkMapWith
            (<>)
            [ (brick, [brick'])
              | brick <- bricks,
                brick' <- bricks,
                brick /= brick',
                brick `f` brick'
            ]
   in both mk (restingOn, flip restingOn)

disintegrateOne :: [Brick] -> Int
disintegrateOne bricks =
  let (brickRestingOn, restingOnBrick) = structure bricks
   in length
        [ brick
          | brick <- bricks,
            brick ∉ restingOnBrick
              || all
                ((&&) <$> (∈ brickRestingOn) <*> ((brickRestingOn |!) >>> (/= mkSet [brick])))
                (restingOnBrick |! brick)
        ]

disintegrateAll :: [Brick] -> Int
disintegrateAll bricks =
  let brickRestingOn = fst $ structure bricks
   in sum
        [ subtract 1 . size . fst $
            iterateFix
              ( \(fallen, bricks) ->
                  let fallen' = setFilter (\brick -> not (onFloor brick) && brickRestingOn |! brick ∖ fallen == (∅)) bricks
                   in (fallen ∪ fallen', bricks ∖ fallen')
              )
              (mkSet [brick], mkSet bricks)
          | brick <- bricks
        ]

fallen :: [Brick]
fallen =
  $(input 22)
    |- (toTuple2 <$$> (toTuple3 <$$$> many1 (((number `sepBy1` char ',') `sepBy1` char '~') <* eol) <* eof))
    & sortOn (thd3 . snd)
    & freefall

part1 :: Int
part1 = disintegrateOne fallen

part2 :: Int
part2 = disintegrateAll fallen
