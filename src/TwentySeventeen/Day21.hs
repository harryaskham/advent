{-# LANGUAGE TupleSections #-}

module TwentySeventeen.Day21 where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

inputPath :: String
inputPath = "input/2017/21.txt"

data Cell = On | Off deriving (Show, Eq, Ord)

fromChar :: Char -> Cell
fromChar '.' = Off
fromChar '#' = On

type Grid = M.Map (Int, Int) Cell

toGrid :: [String] -> Grid
toGrid rows =
  M.fromList
    [ ((x, y), fromChar c)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] row
    ]

maxXY :: M.Map (Int, Int) a -> (Int, Int)
maxXY m = (maximum $ fst <$> M.keys m, maximum $ snd <$> M.keys m)

modifyCoords :: ((Int, Int) -> (Int, Int)) -> Grid -> Grid
modifyCoords f grid = M.mapKeys (fromOrigin . f . toOrigin) grid
  where
    (maxX, maxY) = maxXY grid
    xO = (maxX + 1) `div` 2
    yO = (maxY + 1) `div` 2
    toOrigin (x, y) = (x - xO, y - yO)
    fromOrigin (x, y) = (x + xO, y + yO)

variants :: Grid -> [Grid]
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

parseRule :: String -> [(Grid, Grid)]
parseRule rule = (,rhs) <$> variants lhs
  where
    [lhs, rhs] = toGrid <$> (splitOn "/" <$> splitOn " => " rule)

readRules :: IO (M.Map Grid Grid)
readRules = do
  ls <- lines <$> readFile inputPath
  return . M.fromList $ parseRule =<< ls

splitGrid :: Grid -> M.Map (Int, Int) Grid
splitGrid grid =
  M.fromList
    [ ((gx, gy), subGrid gx gy)
      | gx <- [0 .. ((maxX + 1) `div` d) - 1],
        gy <- [0 .. ((maxY + 1) `div` d) - 1]
    ]
  where
    (maxX, maxY) = maxXY grid
    d = if (maxX + 1) `mod` 2 == 0 then 2 else 3
    subGrid gx gy =
      M.fromList
        [ ((x, y), grid M.! (gx * d + x, gy * d + y))
          | x <- [0 .. d -1],
            y <- [0 .. d -1]
        ]

joinGrid :: M.Map (Int, Int) Grid -> Grid
joinGrid grids =
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

step :: M.Map Grid Grid -> Grid -> Grid
step rules grid = joinGrid expanded
  where
    sGrid = splitGrid grid
    expanded = (rules M.!) <$> sGrid

start :: Grid
start =
  toGrid
    [ ".#.",
      "..#",
      "###"
    ]

part12 :: IO [Int]
part12 = do
  rules <- readRules
  let states = (iterate (step rules) start !!) <$> [5, 18]
  return $ M.size . M.filter (== On) <$> states
