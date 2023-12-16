{-# OPTIONS_GHC -Wno-orphans #-}

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

emptyGrid :: (Griddable Identity g, GridCell a) => g a
emptyGrid = runIdentity emptyGridM

mkGrid :: (GridCell a, Griddable Identity g) => [(Coord2, a)] -> g a
mkGrid = runIdentity . mkGridM

unGrid :: (GridCell a, Griddable Identity g) => g a -> [(Coord2, a)]
unGrid = runIdentity . unGridM

coords :: (GridCell a, Griddable Identity g) => g a -> [Coord2]
coords = runIdentity . coordsM

gridGetMaybe :: (GridCell a, Griddable Identity g) => Coord2 -> g a -> Maybe a
gridGetMaybe c g = runIdentity $ gridGetMaybeM c g

(||?) :: (GridCell a, Griddable Identity g) => g a -> Coord2 -> Maybe a
g ||? c = gridGetMaybe c g

gridGet :: (GridCell a, Griddable Identity g) => Coord2 -> g a -> a
gridGet c g = runIdentity $ gridGetM c g

(||!) :: (GridCell a, Griddable Identity g) => g a -> Coord2 -> a
g ||! c = gridGet c g

gridSet :: (GridCell a, Griddable Identity g) => a -> Coord2 -> g a -> g a
gridSet a c g = runIdentity $ gridSetM a c g

(||.) :: (GridCell a, Griddable Identity g) => g a -> (Coord2, a) -> g a
g ||. (c, a) = gridSet a c g

gridModify :: (GridCell a, Griddable Identity g) => (a -> a) -> Coord2 -> g a -> g a
gridModify f c g = runIdentity $ gridModifyM f c g

(||~) :: (GridCell a, Griddable Identity g) => g a -> (Coord2, a -> a) -> g a
g ||~ (c, f) = gridModify f c g

maxXY :: (GridCell a, Griddable Identity g) => g a -> Coord2
maxXY = runIdentity . maxXYM

minXY :: (GridCell a, Griddable Identity g) => g a -> Coord2
minXY = runIdentity . minXYM

gridFind :: (GridCell a, Griddable Identity g) => a -> g a -> [Coord2]
gridFind a g = runIdentity $ gridFindM a g

gridFindOne :: (GridCell a, Griddable Identity g) => a -> g a -> Coord2
gridFindOne a g = runIdentity $ gridFindOneM a g

fromCoords :: (Foldable f, Bounded a, GridCell a, Griddable Identity g) => a -> f Coord2 -> g a
fromCoords def cs = runIdentity $ fromCoordsM def cs

fillDef :: (GridCell a, Griddable Identity g) => a -> g a -> g a
fillDef def g = runIdentity $ fillDefM def g

fillEmpty :: (Bounded a, GridCell a, Griddable Identity g) => g a -> g a
fillEmpty = runIdentity . fillEmptyM

mapCoords :: (GridCell a, Griddable Identity g) => (Coord2 -> Coord2) -> g a -> g a
mapCoords f g = runIdentity $ mapCoordsM f g

filterCoords :: (GridCell a, Griddable Identity g) => (Coord2 -> Bool) -> g a -> g a
filterCoords f g = runIdentity $ filterCoordsM f g

partitionCoords :: (GridCell a, Griddable Identity g) => (Coord2 -> Bool) -> g a -> (g a, g a)
partitionCoords f g = runIdentity $ partitionCoordsM f g

gridMember :: (GridCell a, Griddable Identity g) => Coord2 -> g a -> Bool
gridMember c g = runIdentity $ gridMemberM c g

(||∈) :: (GridCell a, Griddable Identity g) => Coord2 -> g a -> Bool
a ||∈ g = gridMember a g

(||∉) :: (GridCell a, Griddable Identity g) => Coord2 -> g a -> Bool
a ||∉ g = not (a ||∈ g)

class (Monad m) => Griddable m g where
  emptyGridM :: (GridCell a) => m (g a)
  emptyGridM = mkGridM []
  mkGridM :: (GridCell a) => [(Coord2, a)] -> m (g a)
  unGridM :: (GridCell a) => g a -> m [(Coord2, a)]
  coordsM :: (GridCell a) => g a -> m [Coord2]
  coordsM g = fst <$$> unGridM g
  gridGetMaybeM :: (GridCell a) => Coord2 -> g a -> m (Maybe a)
  (<||?>) :: (GridCell a) => g a -> Coord2 -> m (Maybe a)
  (<||?>) = flip gridGetMaybeM
  gridGetM :: (GridCell a) => Coord2 -> g a -> m a
  (<||!>) :: (GridCell a) => g a -> Coord2 -> m a
  (<||!>) = flip gridGetM
  gridSetM :: (GridCell a) => a -> Coord2 -> g a -> m (g a)
  (<||.>) :: (GridCell a) => g a -> (Coord2, a) -> m (g a)
  g <||.> (c, a) = gridSetM a c g
  gridModifyM :: (GridCell a) => (a -> a) -> Coord2 -> g a -> m (g a)
  (<||~>) :: (GridCell a) => g a -> (Coord2, a -> a) -> m (g a)
  g <||~> (c, f) = gridModifyM f c g
  maxXYM :: (GridCell a) => g a -> m Coord2
  minXYM :: (GridCell a) => g a -> m Coord2
  gridFindM :: (GridCell a) => a -> g a -> m [Coord2]
  gridFindM a g = (\cs -> [k | (k, v) <- cs, v == a]) <$> unGridM g
  gridFindOneM :: (GridCell a) => a -> g a -> m Coord2
  gridFindOneM a g = U.head <$> gridFindM a g
  fromCoordsM :: (Foldable f, Bounded a, GridCell a) => a -> f Coord2 -> m (g a)
  fromCoordsM def cs = do
    e <- emptyGridM
    e' <- foldlM (\g c -> g <||.> (c, def)) e cs
    fillDefM def e'
  fillDefM :: (GridCell a) => a -> g a -> m (g a)
  fillDefM def g = do
    (w, h) <- maxXYM g
    elems <- sequence [(c,) . fromMaybe def <$> (g <||?> c) | x <- [0 .. w], y <- [0 .. h], let c = (x, y)]
    mkGridM elems
  fillEmptyM :: (Bounded a, GridCell a) => g a -> m (g a)
  fillEmptyM = fillDefM minBound
  mapCoordsM :: (GridCell a) => (Coord2 -> Coord2) -> g a -> m (g a)
  mapCoordsM f g = mkGridM =<< (first f <$$> unGridM g)
  filterCoordsM :: (GridCell a) => (Coord2 -> Bool) -> g a -> m (g a)
  filterCoordsM f g = mkGridM . filter (f . fst) =<< unGridM g
  partitionCoordsM :: (GridCell a) => (Coord2 -> Bool) -> g a -> m (g a, g a)
  partitionCoordsM f g = do
    ab <- groupOn (f . fst) <$> unGridM g
    let [a, b] = ab
    gA <- mkGridM a
    gB <- mkGridM b
    return (gA, gB)
  gridMemberM :: (GridCell a) => Coord2 -> g a -> m Bool
  gridMemberM c g = isJust <$> gridGetMaybeM c g
  (<||∈>) :: (GridCell a) => Coord2 -> g a -> m Bool
  a <||∈> g = gridMemberM a g
  (<||∉>) :: (GridCell a) => Coord2 -> g a -> m Bool
  a <||∉> g = not <$> (a <||∈> g)

newtype Grid a = Grid (Map Coord2 a) deriving (Eq, Ord, Show)

instance Griddable Identity Grid where
  mkGridM = pure . Grid . M.fromList
  unGridM (Grid g) = pure $ M.toList g
  gridGetMaybeM c (Grid g) = pure $ M.lookup c g
  gridGetM c (Grid g) = pure $ g M.! c
  gridSetM a c (Grid g) = pure . Grid $ M.insert c a g
  gridModifyM f c (Grid g) = pure . Grid $ M.adjust f c g
  maxXYM (Grid g) = pure (maximum $ fst <$> M.keys g, maximum $ snd <$> M.keys g)
  minXYM (Grid g) = pure (minimum $ fst <$> M.keys g, minimum $ snd <$> M.keys g)
  mapCoordsM f (Grid g) = pure . Grid $ M.mapKeys f g
  filterCoordsM f (Grid g) = pure . Grid $ M.filterWithKey (\k _ -> f k) g
  partitionCoordsM f (Grid g) = pure . both Grid $ M.partitionWithKey (\k _ -> f k) g
  gridMemberM c (Grid g) = pure $ M.member c g

newtype VectorGrid a = VectorGrid (V.Vector (V.Vector a)) deriving (Eq, Ord, Show)

instance Griddable Identity VectorGrid where
  -- TODO: need to respect indices for rotation
  mkGridM cs = pure . VectorGrid . V.fromList $ V.fromList <$> (snd <$$> (fmap (sortOn (fst . fst)) . sortOn (snd . fst . uhead) . groupOn (snd . fst) $ cs))
  unGridM g = do
    (x', y') <- minXYM g
    (x'', y'') <- maxXYM g
    sequence [((x, y),) <$> g <||!> (x, y) | x <- [x' .. x''], y <- [y' .. y'']]
  gridGetMaybeM (x, y) (VectorGrid g) = pure do
    row <- g V.!? y
    row V.!? x
  gridGetM (x, y) (VectorGrid g) = pure $ g V.! y V.! x
  gridSetM a (x, y) (VectorGrid g) =
    let row = g V.! y
     in pure . VectorGrid $ g V.// [(y, row V.// [(x, a)])]
  gridModifyM f (x, y) (VectorGrid g) =
    let row = g V.! y
     in pure . VectorGrid $ g V.// [(y, row V.// [(x, f (row V.! x))])]

  -- Need to modify this to support rotation
  maxXYM (VectorGrid g) = pure (V.length (g V.! 0) - 1, V.length g - 1)
  minXYM _ = pure (0, 0)

-- Relly slow
newtype ArrayGrid a = ArrayGrid (IOArray Coord2 a) deriving (Eq)

instance Griddable IO ArrayGrid where
  mkGridM cs =
    ArrayGrid <$> do
      a <- newArray_ (minimum (fst <$> cs), maximum (fst <$> cs))
      forM_ cs (uncurry $ writeArray a)
      return a
  unGridM (ArrayGrid g) = getAssocs g
  gridGetMaybeM c@(x, y) (ArrayGrid g) = do
    ((ax, ay), (bx, by)) <- getBounds g
    if x < ax || y < ay || x > bx || y > by then return Nothing else Just <$> gridGetM c (ArrayGrid g)
  gridGetM c (ArrayGrid g) = readArray g c
  gridSetM a c (ArrayGrid g) =
    ArrayGrid <$> do
      writeArray g c a
      return g
  gridModifyM f c (ArrayGrid g) =
    ArrayGrid <$> do
      a <- readArray g c
      writeArray g c (f a)
      return g
  maxXYM (ArrayGrid g) = snd <$> getBounds g
  minXYM (ArrayGrid g) = fst <$> getBounds g

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

readGridM :: (GridCell a, Griddable m g) => Text -> m (g a)
readGridM = toGridM . lines

readGrid :: (GridCell a, Griddable Identity g) => Text -> g a
readGrid = toGrid . lines

toGridM :: (GridCell a, Griddable m g) => [Text] -> m (g a)
toGridM rows =
  mkGridM
    [ ((x, y), fromChar c)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] (T.unpack row)
    ]

toGrid :: (GridCell a, Griddable Identity g) => [Text] -> g a
toGrid = runIdentity . toGridM

pointsM :: (GridCell a, Griddable m g) => [Coord2] -> g a -> m [a]
pointsM ps g = catMaybes <$> mapM (g <||?>) ps

points :: (GridCell a, Griddable Identity g) => [Coord2] -> g a -> [a]
points ps g = runIdentity $ pointsM ps g

iterGridM :: (GridCell a, Griddable m g) => Dir2 -> g a -> m [(Coord2, a)]
iterGridM DirDown g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | x <- [0 .. mx]] | y <- [0 .. my]]
iterGridM DirLeft g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | y <- [my, my - 1 .. 0]] | x <- [0 .. mx]]
iterGridM DirRight g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | y <- [0 .. my]] | x <- [0 .. mx]]
iterGridM DirUp g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | x <- [mx, mx - 1 .. 0]] | y <- [my, my - 1 .. 0]]

iterGrid :: (GridCell a, Griddable Identity g) => Dir2 -> g a -> [(Coord2, a)]
iterGrid d g = runIdentity $ iterGridM d g

iterCoordsM :: (GridCell a, Griddable m g) => Dir2 -> g a -> m [Coord2]
iterCoordsM d g = fst <$$> iterGridM d g

iterCoords :: (GridCell a, Griddable Identity g) => Dir2 -> g a -> [Coord2]
iterCoords d g = runIdentity $ iterCoordsM d g

cropXM :: (GridCell a, Griddable m g) => Int -> Int -> g a -> m (g a)
cropXM i j g = do
  g' <- filterCoordsM (\(x, _) -> x >= i && x < j) g
  xO <- fst <$> minXYM g'
  mapCoordsM (first (subtract xO)) g'

cropX :: (GridCell a, Griddable Identity g) => Int -> Int -> g a -> g a
cropX i j g = runIdentity $ cropXM i j g

modifyCoordsM :: (GridCell a, Griddable m g) => (Coord2 -> Coord2) -> g a -> m (g a)
modifyCoordsM f g = do
  (maxX, maxY) <- maxXYM g
  let xO = (maxX + 1) `div` 2
  let yO = (maxY + 1) `div` 2
  let toOrigin (x, y) = (x - xO, y - yO)
  let fromOrigin g =
        do
          (minX, minY) <- minXYM g
          mapCoordsM (\(x, y) -> (x - minX, y - minY)) g
  g' <- mapCoordsM (f . toOrigin) g
  fromOrigin g'

modifyCoords :: (GridCell a, Griddable Identity g) => (Coord2 -> Coord2) -> g a -> g a
modifyCoords f g = runIdentity $ modifyCoordsM f g

variantsNubM :: (GridCell a, Griddable m g, Eq (g a)) => g a -> m [g a]
variantsNubM g = nub <$> variantsM' g

variantsNub :: (GridCell a, Griddable Identity g, Eq (g a)) => g a -> [g a]
variantsNub = runIdentity . variantsNubM

variantsM' :: (GridCell a, Griddable m g) => g a -> m [g a]
variantsM' grid = do
  (maxX, _) <- maxXYM grid
  let isEven = even (maxX + 1)
      flipV (x, y) = (if isEven then negate x - 1 else negate x, y)
      flipH (x, y) = (x, if isEven then negate y - 1 else negate y)
      rot270 (x, y) = (y, if isEven then negate x - 1 else negate x)
      rot180 = rot270 . rot270
      rot90 = rot270 . rot270 . rot270
      mods = (.) <$> [id, flipH, flipV] <*> [id, rot90, rot180, rot270]
  sequence $ modifyCoordsM <$> mods <*> pure grid

variants' :: (GridCell a, Griddable Identity g) => g a -> [g a]
variants' = runIdentity . variantsM'

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

variantsM :: (GridCell a, Griddable m g) => g a -> m (Variants g a)
variantsM grid = do
  vs <- variantsM' grid
  let [a, b, c, d, e, f, g, h, i, j, k, l] = vs
  return $ Variants a b c d e f g h i j k l

variants :: (GridCell a, Griddable Identity g) => g a -> Variants g a
variants = runIdentity . variantsM

-- splitGrid :: (Monad m, GridCell a, Griddable m g) => Int -> Int -> g a -> M.Map Coord2 (g a)
-- splitGrid xStride yStride grid =
--   M.fromList
--     [ ((gx, gy), subGrid gx gy)
--       | gx <- [0 .. ((maxX + 1) `div` xStride) - 1],
--         gy <- [0 .. ((maxY + 1) `div` yStride) - 1]
--     ]
--   where
--     (maxX, maxY) = maxXYM grid
--     subGrid gx gy =
--       mkGridM
--         [ ((x, y), grid ||! (gx * xStride + x, gy * yStride + y))
--           | x <- [0 .. xStride - 1],
--             y <- [0 .. yStride - 1]
--         ]

-- joinGrids :: (Monad m, GridCell a, Griddable m g) => M.Map Coord2 (g a) -> g a
-- joinGrids grids =
--   mkGridM
--     [ ( (gx * (maxX + 1) + x, gy * (maxY + 1) + y),
--         grids M.! (gx, gy) ||! (x, y)
--       )
--       | gx <- [0 .. maxGX],
--         gy <- [0 .. maxGY],
--         x <- [0 .. maxX],
--         y <- [0 .. maxY]
--     ]
--   where
--     (maxGX, maxGY) = maxXYM (Grid (' ' <$ grids))
--     (maxX, maxY) = maxXYM $ grids M.! (0, 0)

gridLinesM :: (GridCell a, Griddable m g) => g a -> m [[a]]
gridLinesM g = do
  (minX, minY) <- minXYM g
  (maxX, maxY) <- maxXYM g
  sequence [sequence [g <||!> (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]

gridLines :: (GridCell a, Griddable Identity g) => g a -> [[a]]
gridLines = runIdentity . gridLinesM

prettyM :: (GridCell a, Griddable m g) => g a -> m Text
prettyM grid = T.pack . intercalate "\n" <$> (fmap toChar <$$> gridLinesM grid)

pretty :: (GridCell a, Griddable Identity g) => g a -> Text
pretty = runIdentity . prettyM

-- -- Extends the grid logically n times in each direction.
-- -- Takes a function used to determine the value given by lookup, which has access to the
-- -- meta coordinates of the grid extension (e.g. x=15, y=15 in an extended 10x10 grid would
-- -- have (1,1) as its extension guide coords.
-- -- Returns the interface to the type, its membership, lookup and bottom-right.
-- extendGrid :: (Monad m, GridCell a, Griddable m g) => Int -> (a -> Coord2 -> b) -> g a -> (Coord2 -> Bool, Coord2 -> b, Coord2)
-- extendGrid n f g = (member, lookup, (w * n - 1, h * n - 1))
--   where
--     (w, h) = both (+ 1) (maxXYM g)
--     lookup (x, y) =
--       let (xi, x') = x `divMod` w
--           (yi, y') = y `divMod` h
--           c = g ||! (x', y')
--        in f c (xi, yi)
--     member (x, y) = x >= 0 && y >= 0 && x < w * n && y < h * n

data Perimeter = Perimeter
  { pTop :: [Coord2],
    pRight :: [Coord2],
    pBottom :: [Coord2],
    pLeft :: [Coord2]
  }
  deriving (Eq, Ord, Show)

pFrom :: Perimeter -> Dir2 -> [Coord2]
pFrom p DirDown = pTop p
pFrom p DirLeft = pRight p
pFrom p DirRight = pLeft p
pFrom p DirUp = pBottom p

perimeterM :: (GridCell a, Griddable m g) => g a -> m Perimeter
perimeterM g = do
  (maxX, maxY) <- maxXYM g
  let top = [(x, 0) | x <- [0 .. maxX]]
  let right = [(maxX, y) | y <- [0 .. maxY]]
  let bottom = [(x, maxY) | x <- [0 .. maxX]]
  let left = [(0, y) | y <- [0 .. maxY]]
  return $ Perimeter top right bottom left

perimeter :: (GridCell a, Griddable Identity g) => g a -> Perimeter
perimeter = runIdentity . perimeterM

instance (GridCell a) => Memberable Coord2 (Grid a) where
  (∈) = (||∈)

instance (GridCell a) => Memberable Coord2 (VectorGrid a) where
  (∈) = (||∈)

instance Unionable (Grid a) where
  (Grid a) ∪ (Grid b) = Grid (a ∪ b)

instance Intersectable (Grid a) where
  (Grid a) ∩ (Grid b) = Grid (a ∩ b)