module Helper.Grid where

import Control.Monad.Memo.Vector (Vector)
import Control.Monad.ST (ST, runST)
import Data.Array (assocs)
import Data.Array.IO (IOArray, getBounds, readArray)
import Data.Array.MArray (MArray, getAssocs, newArray, newArray_, writeArray)
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Fin (Fin)
import Data.List (groupBy)
import Data.List.Extra (groupOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Helper.Collection
import Helper.Coord
import Helper.Tracers
import Helper.Util (Nat10, both, (<$$>))
import Relude.Unsafe qualified as U
import System.IO.Unsafe (unsafePerformIO)

class Griddable g where
  emptyGrid :: (GridCell a) => g a
  emptyGrid = mkGrid []
  mkGrid :: (GridCell a) => [(Coord2, a)] -> g a
  unGrid :: (GridCell a) => g a -> [(Coord2, a)]
  coords :: (GridCell a) => g a -> [Coord2]
  coords = fmap fst . unGrid
  gridGetMaybe :: (GridCell a) => Coord2 -> g a -> Maybe a
  (||?) :: (GridCell a) => g a -> Coord2 -> Maybe a
  (||?) = flip gridGetMaybe
  gridGet :: (GridCell a) => Coord2 -> g a -> a
  (||!) :: (GridCell a) => g a -> Coord2 -> a
  (||!) = flip gridGet
  gridSet :: (GridCell a) => a -> Coord2 -> g a -> g a
  (||.) :: (GridCell a) => g a -> (Coord2, a) -> g a
  g ||. (c, a) = gridSet a c g
  gridModify :: (GridCell a) => (a -> a) -> Coord2 -> g a -> g a
  (||~) :: (GridCell a) => g a -> (Coord2, a -> a) -> g a
  g ||~ (c, f) = gridModify f c g
  maxXY :: (GridCell a) => g a -> Coord2
  minXY :: (GridCell a) => g a -> Coord2
  gridFind :: (GridCell a) => a -> g a -> [Coord2]
  gridFind a g = [k | (k, v) <- unGrid g, v == a]
  gridFindOne :: (GridCell a) => a -> g a -> Coord2
  gridFindOne a g = U.head $ gridFind a g
  fromCoords :: (Foldable f, Bounded a, GridCell a) => a -> f Coord2 -> g a
  fromCoords def = fillEmpty . foldl' (\g c -> g ||. (c, def)) emptyGrid
  fillDef :: (GridCell a) => a -> g a -> g a
  fillDef def g =
    let (w, h) = maxXY g
     in mkGrid [((x, y), v) | x <- [0 .. w], y <- [0 .. h], let v = fromMaybe def (g ||? (x, y))]
  fillEmpty :: (Bounded a, GridCell a) => g a -> g a
  fillEmpty = fillDef minBound
  mapCoords :: (GridCell a) => (Coord2 -> Coord2) -> g a -> g a
  mapCoords f g = mkGrid $ first f <$> unGrid g
  filterCoords :: (GridCell a) => (Coord2 -> Bool) -> g a -> g a
  filterCoords f g = mkGrid $ filter (f . fst) (unGrid g)
  partitionCoords :: (GridCell a) => (Coord2 -> Bool) -> g a -> (g a, g a)
  partitionCoords f g = let [a, b] = groupOn (f . fst) (unGrid g) in both mkGrid (a, b)
  gridMember :: (GridCell a) => Coord2 -> g a -> Bool
  gridMember c g = isJust $ gridGetMaybe c g
  (||∈) :: (GridCell a) => Coord2 -> g a -> Bool
  a ||∈ g = gridMember a g
  (||∉) :: (GridCell a) => Coord2 -> g a -> Bool
  a ||∉ g = not (a ||∈ g)

newtype Grid a = Grid (Map Coord2 a) deriving (Eq, Ord, Show)

instance Griddable Grid where
  mkGrid = Grid . M.fromList
  unGrid (Grid g) = M.toList g
  gridGetMaybe c (Grid g) = M.lookup c g
  gridGet c (Grid g) = g M.! c
  gridSet a c (Grid g) = Grid $ M.insert c a g
  gridModify f c (Grid g) = Grid $ M.adjust f c g
  maxXY (Grid g) = (maximum $ fst <$> M.keys g, maximum $ snd <$> M.keys g)
  minXY (Grid g) = (minimum $ fst <$> M.keys g, minimum $ snd <$> M.keys g)
  mapCoords f (Grid g) = Grid $ M.mapKeys f g
  filterCoords f (Grid g) = Grid $ M.filterWithKey (\k _ -> f k) g
  partitionCoords f (Grid g) = both Grid $ M.partitionWithKey (\k _ -> f k) g
  gridMember c (Grid g) = M.member c g

newtype VectorGrid a = VectorGrid (V.Vector (V.Vector a)) deriving (Eq, Ord, Show)

instance Griddable VectorGrid where
  mkGrid cs = VectorGrid . V.fromList $ V.fromList <$> (snd <$$> (fmap (sortOn (fst . fst)) . sortOn (snd . fst . uhead) . groupOn (snd . fst) $ cs))
  unGrid g =
    [ ((x, y), g ||! (x, y))
      | let (x', y') = minXY g,
        let (x'', y'') = maxXY g,
        x <- [x' .. x''],
        y <- [y' .. y'']
    ]
  gridGetMaybe (x, y) (VectorGrid g) = do
    row <- g V.!? y
    row V.!? x
  gridGet (x, y) (VectorGrid g) = g V.! y V.! x
  gridSet a (x, y) (VectorGrid g) =
    let row = g V.! y
     in VectorGrid $ g V.// [(y, row V.// [(x, a)])]
  gridModify f (x, y) (VectorGrid g) =
    let row = g V.! y
     in VectorGrid $ g V.// [(y, row V.// [(x, f (row V.! x))])]
  maxXY (VectorGrid g) = (V.length (g V.! 0) - 1, V.length g - 1)
  minXY _ = (0, 0)

-- newtype STArrayGrid s a = STArrayGrid (ST s (STArray s Coord2 a))

-- mkArrayGrid :: (MArray a1 (a2 i e) (STArray s Coord2), MArray a2 e (a1 i)) => [(i, a2 i e)] -> STArrayGrid s (a1 i (a2 i e))
-- mkArrayGrid cs =
--   let bounds = (minimum (fst <$> cs), maximum (fst <$> cs))
--    in STArrayGrid . return $ foldlM (\a (c, v) -> writeArray a c v >> return a) (newArray_ bounds) cs

-- unArrayGrid :: STArrayGrid s a -> ST s [(Coord2, a)]
-- unArrayGrid (STArrayGrid g) = getAssocs =<< g

newtype ArrayGrid a = ArrayGrid (IO (IOArray Coord2 a))

instance Griddable ArrayGrid where
  mkGrid cs =
    ArrayGrid do
      a <- newArray_ (minimum (fst <$> cs), maximum (fst <$> cs))
      forM_ cs (uncurry $ writeArray a)
      return a
  unGrid (ArrayGrid g) = unsafePerformIO $ getAssocs =<< g
  gridGetMaybe c (ArrayGrid g)
    | let (a, b) = unsafePerformIO (getBounds =<< g), c < a || c > b = Nothing
    | otherwise = Just . unsafePerformIO $ flip readArray c =<< g
  gridGet c (ArrayGrid g) = unsafePerformIO $ flip readArray c =<< g
  gridSet a c (ArrayGrid g') = ArrayGrid do
    g <- g'
    writeArray g c a
    return g
  gridModify f c (ArrayGrid g') = ArrayGrid do
    g <- g'
    a <- readArray g c
    writeArray g c (f a)
    return g
  maxXY (ArrayGrid g) = snd . unsafePerformIO $ getBounds =<< g
  minXY (ArrayGrid g) = fst . unsafePerformIO $ getBounds =<< g

instance (GridCell a) => Eq (ArrayGrid a) where
  a == b = mkSet (unGrid a) == mkSet (unGrid b)

instance (GridCell a) => Ord (ArrayGrid a) where
  compare a b = compare (mkSet $ unGrid a) (mkSet $ unGrid b)

-- To create a Cell, just supply a Bimap between char and cell
-- Or, one can override toChar and fromChar where there is some special logic
class (Ord a) => GridCell a where
  charMap :: Bimap a Char
  fromChar :: Char -> a
  fromChar c = charMap BM.!> c
  toChar :: a -> Char
  toChar a = charMap BM.! a

data DotHash = Dot | Hash deriving (Eq, Ord, Bounded, Show)

instance GridCell Char where
  fromChar = id
  toChar = id
  charMap = BM.empty

instance GridCell DotHash where
  charMap = BM.fromList [(Dot, '.'), (Hash, '#')]

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

readGrid :: (GridCell a, Griddable g) => Text -> g a
readGrid = toGrid . lines

toGrid :: (GridCell a, Griddable g) => [Text] -> g a
toGrid rows =
  mkGrid
    [ ((x, y), fromChar c)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] (T.unpack row)
    ]

points :: (GridCell a, Griddable g) => [Coord2] -> g a -> [a]
points ps g = mapMaybe (g ||?) ps

iterGrid :: (GridCell a, Griddable g) => Dir2 -> g a -> [(Coord2, a)]
iterGrid DirDown g = mconcat [[((x, y), g ||! (x, y)) | x <- [0 .. mx]] | let (mx, my) = maxXY g, y <- [0 .. my]]
iterGrid DirLeft g = mconcat [[((x, y), g ||! (x, y)) | y <- [my, my - 1 .. 0]] | let (mx, my) = maxXY g, x <- [0 .. mx]]
iterGrid DirRight g = mconcat [[((x, y), g ||! (x, y)) | y <- [0 .. my]] | let (mx, my) = maxXY g, x <- [0 .. mx]]
iterGrid DirUp g = mconcat [[((x, y), g ||! (x, y)) | x <- [mx, mx - 1 .. 0]] | let (mx, my) = maxXY g, y <- [my, my - 1 .. 0]]

iterCoords :: (GridCell a, Griddable g) => Dir2 -> g a -> [Coord2]
iterCoords d g = fst <$> iterGrid d g

cropX :: (GridCell a, Griddable g) => Int -> Int -> g a -> g a
cropX i j g =
  let g' = filterCoords (\(x, _) -> x >= i && x < j) g
      xO = fst $ minXY g'
   in mapCoords (first (subtract xO)) g'

modifyCoords :: (GridCell a, Griddable g) => (Coord2 -> Coord2) -> g a -> g a
modifyCoords f g = fromOrigin $ mapCoords (f . toOrigin) g
  where
    (maxX, maxY) = maxXY g
    xO = (maxX + 1) `div` 2
    yO = (maxY + 1) `div` 2
    toOrigin (x, y) = (x - xO, y - yO)
    fromOrigin m = let (xO', yO') = both negate (minXY m) in mapCoords (\(x, y) -> (x + xO', y + yO')) m

variantsNub :: (GridCell a, Griddable g, Eq (g a)) => g a -> [g a]
variantsNub g = nub (variants' g)

variants' :: (GridCell a, Griddable g) => g a -> [g a]
variants' grid = modifyCoords <$> mods <*> pure grid
  where
    (maxX, _) = maxXY grid
    isEven = even (maxX + 1)
    flipV (x, y) = (if isEven then negate x - 1 else negate x, y)
    flipH (x, y) = (x, if isEven then negate y - 1 else negate y)
    rot270 (x, y) = (y, if isEven then negate x - 1 else negate x)
    rot180 = rot270 . rot270
    rot90 = rot270 . rot270 . rot270
    mods = (.) <$> [id, flipH, flipV] <*> [id, rot90, rot180, rot270]

data Variants g a = Variants
  { vId :: g a,
    r90 :: g a,
    r180 :: g a,
    r270 :: g a,
    h0 :: g a,
    h90 :: g a,
    h180 :: g a,
    h270 :: g a,
    v0 :: g a,
    v90 :: g a,
    v180 :: g a,
    v270 :: g a
  }

variants :: (Griddable g, GridCell a) => g a -> Variants g a
variants grid =
  let [a, b, c, d, e, f, g, h, i, j, k, l] = variants' grid
   in Variants a b c d e f g h i j k l

splitGrid :: (Griddable g, GridCell a) => Int -> Int -> g a -> M.Map Coord2 (g a)
splitGrid xStride yStride grid =
  M.fromList
    [ ((gx, gy), subGrid gx gy)
      | gx <- [0 .. ((maxX + 1) `div` xStride) - 1],
        gy <- [0 .. ((maxY + 1) `div` yStride) - 1]
    ]
  where
    (maxX, maxY) = maxXY grid
    subGrid gx gy =
      mkGrid
        [ ((x, y), grid ||! (gx * xStride + x, gy * yStride + y))
          | x <- [0 .. xStride - 1],
            y <- [0 .. yStride - 1]
        ]

joinGrids :: (Griddable g, GridCell a) => M.Map Coord2 (g a) -> g a
joinGrids grids =
  mkGrid
    [ ( (gx * (maxX + 1) + x, gy * (maxY + 1) + y),
        grids M.! (gx, gy) ||! (x, y)
      )
      | gx <- [0 .. maxGX],
        gy <- [0 .. maxGY],
        x <- [0 .. maxX],
        y <- [0 .. maxY]
    ]
  where
    (maxGX, maxGY) = maxXY (Grid (' ' <$ grids))
    (maxX, maxY) = maxXY $ grids M.! (0, 0)

pretty :: (GridCell a, Griddable g) => g a -> Text
pretty grid =
  T.pack $
    intercalate
      "\n"
      [[toChar $ grid ||! (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    (minX, minY) = minXY grid
    (maxX, maxY) = maxXY grid

prettyPrint :: (GridCell a) => Grid a -> IO ()
prettyPrint = putTextLn . pretty

-- Extends the grid logically n times in each direction.
-- Takes a function used to determine the value given by lookup, which has access to the
-- meta coordinates of the grid extension (e.g. x=15, y=15 in an extended 10x10 grid would
-- have (1,1) as its extension guide coords.
-- Returns the interface to the type, its membership, lookup and bottom-right.
extendGrid :: (GridCell a, Griddable g) => Int -> (a -> Coord2 -> b) -> g a -> (Coord2 -> Bool, Coord2 -> b, Coord2)
extendGrid n f g = (member, lookup, (w * n - 1, h * n - 1))
  where
    (w, h) = both (+ 1) (maxXY g)
    lookup (x, y) =
      let (xi, x') = x `divMod` w
          (yi, y') = y `divMod` h
          c = g ||! (x', y')
       in f c (xi, yi)
    member (x, y) = x >= 0 && y >= 0 && x < w * n && y < h * n