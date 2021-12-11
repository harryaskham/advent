module Helper.Grid where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Char (intToDigit)
import Data.Fin (Fin)
import Data.List (intercalate, maximum, minimum, nub)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Type.Nat (Nat (S), Nat9)

-- To create a Cell, just supply a Bimap between char and cell
-- Or, one can override toChar and fromChar where there is some special logic
class Ord a => GridCell a where
  charMap :: Bimap a Char
  fromChar :: Char -> a
  fromChar c = charMap BM.!> c
  toChar :: a -> Char
  toChar a = charMap BM.! a

newtype DigitCell = DigitCell (Fin ('S Nat9)) deriving (Eq, Ord, Bounded, Num, Show)

instance GridCell DigitCell where
  charMap = BM.fromList [(DigitCell (fromInteger i), intToDigit (fromIntegral i)) | i <- [0 .. 9]]

type Grid a = M.Map (Int, Int) a

-- Helper for reading a Grid from an input file
readGridIO :: GridCell a => FilePath -> IO (Grid a)
readGridIO path = readGrid <$> readFileText path

readGrid :: GridCell a => Text -> Grid a
readGrid = toGrid . lines

toGrid :: GridCell a => [Text] -> Grid a
toGrid rows =
  M.fromList
    [ ((x, y), fromChar c)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] (T.unpack row)
    ]

points :: [(Int, Int)] -> Grid a -> [a]
points ps g = catMaybes $ M.lookup <$> ps <*> pure g

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
    isEven = even (maxX + 1)
    flipH (x, y) = (if isEven then negate x - 1 else negate x, y)
    flipV (x, y) = (x, if isEven then negate y - 1 else negate y)
    rot90 (x, y) = (y, if isEven then negate x - 1 else negate x)
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

pretty :: GridCell a => Grid a -> Text
pretty grid =
  T.pack $
    intercalate
      "\n"
      [[toChar $ grid M.! (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]]
  where
    (maxX, maxY) = maxXY grid

prettyPrint :: GridCell a => Grid a -> IO ()
prettyPrint = putTextLn . pretty
