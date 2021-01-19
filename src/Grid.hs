module Grid where

import Data.List (intercalate, nub)
import qualified Data.Map.Strict as M

type Grid a = M.Map (Int, Int) a

toGrid :: (Char -> a) -> [String] -> Grid a
toGrid fromChar rows =
  M.fromList
    [ ((x, y), fromChar c)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] row
    ]

maxXY :: M.Map (Int, Int) a -> (Int, Int)
maxXY m = (maximum $ fst <$> M.keys m, maximum $ snd <$> M.keys m)

minXY :: M.Map (Int, Int) a -> (Int, Int)
minXY m = (minimum $ fst <$> M.keys m, minimum $ snd <$> M.keys m)

modifyCoords :: ((Int, Int) -> (Int, Int)) -> Grid a -> Grid a
modifyCoords f grid = M.mapKeys (fromOrigin . f . toOrigin) grid
  where
    (maxX, maxY) = maxXY grid
    xO = (maxX + 1) `div` 2
    yO = (maxY + 1) `div` 2
    toOrigin (x, y) = (x - xO, y - yO)
    fromOrigin (x, y) = (x + xO, y + yO)

variants :: Eq a => Grid a -> [Grid a]
variants grid = nub $ modifyCoords <$> mods <*> pure grid
  where
    (maxX, _) = maxXY grid
    even = (maxX + 1) `mod` 2 == 0
    flipH (x, y) = (if even then negate x - 1 else negate x, y)
    flipV (x, y) = (x, if even then negate y - 1 else negate y)
    rot90 (x, y) = (y, if even then negate x - 1 else negate x)
    rot180 = rot90 . rot90
    rot270 = rot90 . rot90 . rot90
    mods = (.) <$> [id, flipH, flipV] <*> [id, rot90, rot180, rot270]

splitGrid :: Int -> Int -> Grid a -> M.Map (Int, Int) (Grid a)
splitGrid xStride yStride grid =
  M.fromList
    [ ((gx, gy), subGrid gx gy)
      | gx <- [0 .. ((maxX + 1) `div` xStride) - 1],
        gy <- [0 .. ((maxY + 1) `div` yStride) - 1]
    ]
  where
    (maxX, maxY) = maxXY grid
    subGrid gx gy =
      M.fromList
        [ ((x, y), grid M.! (gx * xStride + x, gy * yStride + y))
          | x <- [0 .. xStride - 1],
            y <- [0 .. yStride - 1]
        ]

joinGrids :: M.Map (Int, Int) (Grid a) -> Grid a
joinGrids grids =
  M.fromList
    [ ( (gx * (maxX + 1) + x, gy * (maxY + 1) + y),
        grids M.! (gx, gy) M.! (x, y)
      )
      | gx <- [0 .. maxGX],
        gy <- [0 .. maxGY],
        x <- [0 .. maxX],
        y <- [0 .. maxY]
    ]
  where
    (maxGX, maxGY) = maxXY grids
    (maxX, maxY) = maxXY $ grids M.! (0, 0)

pretty :: Show a => Grid a -> String
pretty grid = intercalate "\n" [concat [show $ grid M.! (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]]
  where
    (maxX, maxY) = maxXY grid
