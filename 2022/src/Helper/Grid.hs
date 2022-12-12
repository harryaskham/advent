module Helper.Grid where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Char (intToDigit)
import Data.Fin (Fin)
import Data.List (intercalate, maximum, minimum, nub)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Helper.Util (Nat10, both)
import Relude.Unsafe qualified as U
import Prelude hiding (find)

-- To create a Cell, just supply a Bimap between char and cell
-- Or, one can override toChar and fromChar where there is some special logic
class Ord a => GridCell a where
  charMap :: Bimap a Char
  fromChar :: Char -> a
  fromChar c = charMap BM.!> c
  toChar :: a -> Char
  toChar a = charMap BM.! a

data SimpleWall = Empty | Wall deriving (Eq, Ord, Bounded, Show)

instance GridCell SimpleWall where
  charMap = BM.fromList [(Empty, ' '), (Wall, '#')]

newtype DigitCell = DigitCell (Fin Nat10) deriving (Eq, Ord, Bounded, Num, Show)

cellToInt :: DigitCell -> Integer
cellToInt (DigitCell d) = toInteger d

intToCell :: Int -> DigitCell
intToCell d = DigitCell (fromInteger $ toInteger d)

instance GridCell DigitCell where
  charMap = BM.fromList [(DigitCell (fromInteger i), intToDigit (fromIntegral i)) | i <- [0 .. 9]]

newtype IntCell = IntCell Int deriving (Eq, Ord, Num, Show)

instance GridCell IntCell where
  charMap = BM.fromList [(IntCell i, intToDigit i) | i <- [0 .. 9]]

type Grid a = M.Map (Int, Int) a

-- Helper for reading a Grid from an input file
readGridIO :: GridCell a => FilePath -> IO (Grid a)
readGridIO path = readGrid . decodeUtf8 @Text <$> readFileBS path

readGrid :: GridCell a => Text -> Grid a
readGrid = toGrid . lines

fromCoords :: (Foldable f, Bounded a) => a -> f (Int, Int) -> Grid a
fromCoords def = fillEmpty . foldl' (\g c -> M.insert c def g) M.empty

fillDef :: a -> Grid a -> Grid a
fillDef def g =
  let (w, h) = maxXY g
   in M.fromList [((x, y), v) | x <- [0 .. w], y <- [0 .. h], let v = fromMaybe def (M.lookup (x, y) g)]

fillEmpty :: (Bounded a) => Grid a -> Grid a
fillEmpty = fillDef minBound

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
      [[toChar $ grid M.! (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    (minX, minY) = minXY grid
    (maxX, maxY) = maxXY grid

prettyPrint :: GridCell a => Grid a -> IO ()
prettyPrint = putTextLn . pretty

-- Extends the grid logically n times in each direction.
-- Takes a function used to determine the value given by lookup, which has access to the
-- meta coordinates of the grid extension (e.g. x=15, y=15 in an extended 10x10 grid would
-- have (1,1) as its extension guide coords.
-- Returns the interface to the type, its membership, lookup and bottom-right.
extendGrid :: Int -> (a -> (Int, Int) -> b) -> Grid a -> ((Int, Int) -> Bool, (Int, Int) -> b, (Int, Int))
extendGrid n f g = (member, lookup, (w * n - 1, h * n - 1))
  where
    (w, h) = both (+ 1) (maxXY g)
    lookup (x, y) =
      let (xi, x') = x `divMod` w
          (yi, y') = y `divMod` h
          c = g M.! (x', y')
       in f c (xi, yi)
    member (x, y) = x >= 0 && y >= 0 && x < w * n && y < h * n

find :: (Eq a) => a -> Grid a -> [(Int, Int)]
find a g = [k | (k, v) <- M.toList g, v == a]

findOne :: (Eq a) => a -> Grid a -> (Int, Int)
findOne a g = U.head $ find a g

rowsCols :: Grid a -> ([[(Int, Int)]], [[(Int, Int)]])
rowsCols g = (rows, cols)
  where
    (w, h) = maxXY g
    rows = [[(x, y) | x <- [0 .. w]] | y <- [0 .. h]]
    cols = [[(x, y) | y <- [0 .. h]] | x <- [0 .. w]]
