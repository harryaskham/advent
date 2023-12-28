{-# OPTIONS_GHC -Wno-orphans #-}

module Helper.Grid where

import Control.Monad.Memo.Vector (Vector)
import Control.Monad.ST (ST, runST)
import Data.Array (assocs)
import Data.Array.IO (IOArray, getBounds, readArray)
import Data.Array.MArray (MArray, getAssocs, newArray, newArray_, writeArray)
import Data.Array.ST qualified as STA
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

emptyGrid :: (Griddable Identity g k a) => g k a
emptyGrid = runIdentity emptyGridM

mkGrid :: (Griddable Identity g k a) => [(k, a)] -> g k a
mkGrid = runIdentity . mkGridM

unGrid :: (Griddable Identity g k a) => g k a -> [(k, a)]
unGrid = runIdentity . unGridM

coords :: (Griddable Identity g k a) => g k a -> [k]
coords = runIdentity . coordsM

gridGetMaybe :: (Griddable Identity g k a) => k -> g k a -> Maybe a
gridGetMaybe c g = runIdentity $ gridGetMaybeM c g

(||?) :: (Griddable Identity g k a) => g k a -> k -> Maybe a
g ||? c = gridGetMaybe c g

gridGet :: (Griddable Identity g k a) => k -> g k a -> a
gridGet c g = runIdentity $ gridGetM c g

(||!) :: (Griddable Identity g k a) => g k a -> k -> a
g ||! c = gridGet c g

gridSet :: (Griddable Identity g k a) => a -> k -> g k a -> g k a
gridSet a c g = runIdentity $ gridSetM a c g

(||.) :: (Griddable Identity g k a) => g k a -> (k, a) -> g k a
g ||. (c, a) = gridSet a c g

gridModify :: (Griddable Identity g k a) => (a -> a) -> k -> g k a -> g k a
gridModify f c g = runIdentity $ gridModifyM f c g

(||~) :: (Griddable Identity g k a) => g k a -> (k, a -> a) -> g k a
g ||~ (c, f) = gridModify f c g

maxXY :: (Griddable Identity g k a) => g k a -> (Int, Int)
maxXY = runIdentity . maxXYM

minXY :: (Griddable Identity g k a) => g k a -> (Int, Int)
minXY = runIdentity . minXYM

gridFind :: (Griddable Identity g k a) => a -> g k a -> [k]
gridFind a g = runIdentity $ gridFindM a g

gridFindOne :: (Griddable Identity g k a) => a -> g k a -> k
gridFindOne a g = runIdentity $ gridFindOneM a g

fromCoords :: (Foldable f, Bounded a, Griddable Identity g k a) => a -> f k -> g k a
fromCoords def cs = runIdentity $ fromCoordsM def cs

fillDef :: (Griddable Identity g k a) => a -> g k a -> g k a
fillDef def g = runIdentity $ fillDefM def g

fillEmpty :: (Bounded a, Griddable Identity g k a) => g k a -> g k a
fillEmpty = runIdentity . fillEmptyM

mapCoords :: (Griddable Identity g k a) => (k -> k) -> g k a -> g k a
mapCoords f g = runIdentity $ mapCoordsM f g

filterCoords :: (Griddable Identity g k a) => (k -> Bool) -> g k a -> g k a
filterCoords f g = runIdentity $ filterCoordsM f g

partitionCoords :: (Griddable Identity g k a) => (k -> Bool) -> g k a -> (g k a, g k a)
partitionCoords f g = runIdentity $ partitionCoordsM f g

gridMember :: (Griddable Identity g k a) => k -> g k a -> Bool
gridMember c g = runIdentity $ gridMemberM c g

class (Monad m, Coord k, GridCell a) => Griddable m g k a where
  emptyGridM :: m (g k a)
  emptyGridM = mkGridM []
  mkGridM :: [(k, a)] -> m (g k a)
  unGridM :: g k a -> m [(k, a)]
  coordsM :: g k a -> m [k]
  coordsM g = fst <$$> unGridM g
  gridGetMaybeM :: k -> g k a -> m (Maybe a)
  (<||?>) :: g k a -> k -> m (Maybe a)
  (<||?>) = flip gridGetMaybeM
  gridGetM :: k -> g k a -> m a
  (<||!>) :: g k a -> k -> m a
  (<||!>) = flip gridGetM
  gridSetM :: a -> k -> g k a -> m (g k a)
  (<||.>) :: g k a -> (k, a) -> m (g k a)
  g <||.> (c, a) = gridSetM a c g
  gridModifyM :: (a -> a) -> k -> g k a -> m (g k a)
  (<||~>) :: g k a -> (k, a -> a) -> m (g k a)
  g <||~> (c, f) = gridModifyM f c g
  maxXYM :: g k a -> m (Int, Int)
  minXYM :: g k a -> m (Int, Int)
  gridFindM :: a -> g k a -> m [k]
  gridFindM a g = (\cs -> [k | (k, v) <- cs, v == a]) <$> unGridM g
  gridFindOneM :: a -> g k a -> m k
  gridFindOneM a g = U.head <$> gridFindM a g
  fromCoordsM :: (Foldable f, Bounded a) => a -> f k -> m (g k a)
  fromCoordsM def cs = do
    e <- emptyGridM
    e' <- foldlM (\g c -> g <||.> (c, def)) e cs
    fillDefM def e'
  fillDefM :: a -> g k a -> m (g k a)
  fillDefM def g = do
    cs <- coordsM g
    elems <- sequence [(c,) . fromMaybe def <$> (g <||?> c) | c <- cs]
    mkGridM elems
  fillEmptyM :: (Bounded a) => g k a -> m (g k a)
  fillEmptyM = fillDefM minBound
  mapCoordsM :: (k -> k) -> g k a -> m (g k a)
  mapCoordsM f g = mkGridM =<< (first f <$$> unGridM g)
  filterCoordsM :: (k -> Bool) -> g k a -> m (g k a)
  filterCoordsM f g = mkGridM . filter (f . fst) =<< unGridM g
  partitionCoordsM :: (k -> Bool) -> g k a -> m (g k a, g k a)
  partitionCoordsM f g = do
    ab <- groupOn (f . fst) <$> unGridM g
    let [a, b] = ab
    gA <- mkGridM a
    gB <- mkGridM b
    return (gA, gB)
  gridMemberM :: k -> g k a -> m Bool
  gridMemberM c g = isJust <$> gridGetMaybeM c g
  (<||∈>) :: k -> g k a -> m Bool
  a <||∈> g = gridMemberM a g
  (<||∉>) :: k -> g k a -> m Bool
  a <||∉> g = not <$> (a <||∈> g)

newtype Grid' k a = Grid (Map k a) deriving (Eq, Ord, Show)

type Grid = Grid' Coord2

instance (GridCell a) => Griddable Identity Grid' Coord2 a where
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

newtype VectorGrid' k a = VectorGrid (V.Vector (V.Vector a)) deriving (Eq, Ord, Show)

type VectorGrid = VectorGrid' Coord2

instance (GridCell a) => Griddable Identity VectorGrid' Coord2 a where
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

newtype ArrayGrid' k a = ArrayGrid (IOArray k a) deriving (Eq)

type ArrayGrid a = ArrayGrid' Coord2 a

instance (GridCell a) => Griddable IO ArrayGrid' Coord2 a where
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

newtype STUArrayGrid' s k a = STUArrayGrid (STA.STUArray s k a) deriving (Eq)

type STUArrayGrid s a = STUArrayGrid' s Coord2 a

instance (GridCell a, MArray (STA.STUArray s) a (ST s)) => Griddable (ST s) (STUArrayGrid' s) Coord2 a where
  mkGridM cs =
    STUArrayGrid <$> do
      a <- newArray_ (minimum (fst <$> cs), maximum (fst <$> cs))
      forM_ cs (uncurry $ writeArray a)
      return a
  unGridM (STUArrayGrid g) = getAssocs g
  gridGetMaybeM c@(x, y) (STUArrayGrid g) = do
    ((ax, ay), (bx, by)) <- getBounds g
    if x < ax || y < ay || x > bx || y > by then return Nothing else Just <$> gridGetM c (STUArrayGrid g)
  gridGetM c (STUArrayGrid g) = readArray g c
  gridSetM a c (STUArrayGrid g) =
    STUArrayGrid <$> do
      writeArray g c a
      return g
  gridModifyM f c (STUArrayGrid g) =
    STUArrayGrid <$> do
      a <- readArray g c
      writeArray g c (f a)
      return g
  maxXYM (STUArrayGrid g) = snd <$> getBounds g
  minXYM (STUArrayGrid g) = fst <$> getBounds g

newtype STArrayGrid' s k a = STArrayGrid (STA.STArray s k a) deriving (Eq)

type STArrayGrid s a = STArrayGrid' s Coord2 a

instance (GridCell a, MArray (STA.STArray s) a (ST s)) => Griddable (ST s) (STArrayGrid' s) Coord2 a where
  mkGridM cs =
    STArrayGrid <$> do
      a <- newArray_ (minimum (fst <$> cs), maximum (fst <$> cs))
      forM_ cs (uncurry $ writeArray a)
      return a
  unGridM (STArrayGrid g) = getAssocs g
  gridGetMaybeM c@(x, y) (STArrayGrid g) = do
    ((ax, ay), (bx, by)) <- getBounds g
    if x < ax || y < ay || x > bx || y > by then return Nothing else Just <$> gridGetM c (STArrayGrid g)
  gridGetM c (STArrayGrid g) = readArray g c
  gridSetM a c (STArrayGrid g) =
    STArrayGrid <$> do
      writeArray g c a
      return g
  gridModifyM f c (STArrayGrid g) =
    STArrayGrid <$> do
      a <- readArray g c
      writeArray g c (f a)
      return g
  maxXYM (STArrayGrid g) = snd <$> getBounds g
  minXYM (STArrayGrid g) = fst <$> getBounds g

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

instance GridCell Int where
  fromChar = digitToInt
  toChar = intToDigit
  charMap = BM.empty

instance GridCell Bool where
  charMap = mkBimap [(True, 'T'), (False, 'F')]

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

readGridM :: (Griddable m g k a) => Text -> m (g k a)
readGridM = toGridM . lines

readGrid :: (Griddable Identity g k a) => Text -> g k a
readGrid = toGrid . lines

toGridM :: (Griddable m g k a) => [Text] -> m (g k a)
toGridM rows =
  mkGridM
    [ (fromXY (x, y), fromChar c)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] (T.unpack row)
    ]

toGrid :: (Griddable Identity g k a) => [Text] -> g k a
toGrid = runIdentity . toGridM

pointsM :: (Griddable m g k a) => [k] -> g k a -> m [a]
pointsM ps g = catMaybes <$> mapM (g <||?>) ps

points :: (Griddable Identity g k a) => [k] -> g k a -> [a]
points ps g = runIdentity $ pointsM ps g

iterGridM :: (Griddable m g Coord2 a) => Dir2 -> g Coord2 a -> m [(Coord2, a)]
iterGridM DirDown g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | x <- [0 .. mx]] | y <- [0 .. my]]
iterGridM DirLeft g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | y <- [my, my - 1 .. 0]] | x <- [0 .. mx]]
iterGridM DirRight g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | y <- [0 .. my]] | x <- [0 .. mx]]
iterGridM DirUp g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | x <- [mx, mx - 1 .. 0]] | y <- [my, my - 1 .. 0]]

iterGrid :: (Griddable Identity g Coord2 a) => Dir2 -> g Coord2 a -> [(Coord2, a)]
iterGrid d g = runIdentity $ iterGridM d g

iterCoordsM :: (Griddable m g Coord2 a) => Dir2 -> g Coord2 a -> m [Coord2]
iterCoordsM d g = fst <$$> iterGridM d g

iterCoords :: (Griddable Identity g Coord2 a) => Dir2 -> g Coord2 a -> [Coord2]
iterCoords d g = runIdentity $ iterCoordsM d g

cropXM :: (Griddable m g k a) => Int -> Int -> g k a -> m (g k a)
cropXM i j g = do
  g' <- filterCoordsM (\c -> let (x, _) = toXY c in x >= i && x < j) g
  xO <- fst <$> minXYM g'
  mapCoordsM (mapXY $ first (subtract xO)) g'

cropX :: (Griddable Identity g k a) => Int -> Int -> g k a -> g k a
cropX i j g = runIdentity $ cropXM i j g

modifyCoordsM :: (Griddable m g k a) => (k -> k) -> g k a -> m (g k a)
modifyCoordsM f g = do
  (maxX, maxY) <- maxXYM g
  let xO = (maxX + 1) `div` 2
  let yO = (maxY + 1) `div` 2
  let toOrigin c = let (x, y) = toXY c in fromXY (x - xO, y - yO)
  let fromOrigin g =
        do
          (minX, minY) <- minXYM g
          mapCoordsM (\c -> let (x, y) = toXY c in fromXY (x - minX, y - minY)) g
  g' <- mapCoordsM (f . toOrigin) g
  fromOrigin g'

modifyCoords :: (Griddable Identity g k a) => (k -> k) -> g k a -> g k a
modifyCoords f g = runIdentity $ modifyCoordsM f g

variantsNubM :: (Griddable m g k a, Eq (g k a)) => g k a -> m [g k a]
variantsNubM g = nub <$> variantsM' g

variantsNub :: (Griddable Identity g k a, Eq (g k a)) => g k a -> [g k a]
variantsNub = runIdentity . variantsNubM

variantsM' :: (Griddable m g k a) => g k a -> m [g k a]
variantsM' grid = do
  (maxX, _) <- maxXYM grid
  let isEven = even (maxX + 1)
      flipV (x, y) = (if isEven then negate x - 1 else negate x, y)
      flipH (x, y) = (x, if isEven then negate y - 1 else negate y)
      rot270 (x, y) = (y, if isEven then negate x - 1 else negate x)
      rot180 = rot270 . rot270
      rot90 = rot270 . rot270 . rot270
      mods = (.) <$> [id, flipH, flipV] <*> [id, rot90, rot180, rot270]
  sequence $ modifyCoordsM <$> (mapXY <$> mods) <*> pure grid

variants' :: (Griddable Identity g k a) => g k a -> [g k a]
variants' = runIdentity . variantsM'

data Variants g k a = Variants
  { vId :: g k a,
    r90 :: g k a,
    r180 :: g k a,
    r270 :: g k a,
    h0 :: g k a,
    h90 :: g k a,
    h180 :: g k a,
    h270 :: g k a,
    v0 :: g k a,
    v90 :: g k a,
    v180 :: g k a,
    v270 :: g k a
  }

variantsM :: (Griddable m g k a) => g k a -> m (Variants g k a)
variantsM grid = do
  vs <- variantsM' grid
  let [a, b, c, d, e, f, g, h, i, j, k, l] = vs
  return $ Variants a b c d e f g h i j k l

variants :: (Griddable Identity g k a) => g k a -> Variants g k a
variants = runIdentity . variantsM

gridLinesM :: (Griddable m g k a) => g k a -> m [[a]]
gridLinesM g = do
  (minX, minY) <- minXYM g
  (maxX, maxY) <- maxXYM g
  sequence [sequence [g <||!> fromXY (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]

gridLines :: (Griddable Identity g k a) => g k a -> [[a]]
gridLines = runIdentity . gridLinesM

prettyM :: (Griddable m g k a) => g k a -> m Text
prettyM grid = T.pack . intercalate "\n" <$> (fmap toChar <$$> gridLinesM grid)

pretty :: (Griddable Identity g k a) => g k a -> Text
pretty = runIdentity . prettyM

convolve :: (Griddable Identity g Coord2 c, Griddable Identity g Coord2 d) => (Int, Int, Int, Int) -> (g Coord2 c -> d) -> g Coord2 c -> g Coord2 d
convolve (u, d, l, r) f g =
  mkGrid
    [ (c, f g')
      | c@(x', y') <- coords g,
        let g' = mapCoords (\(x, y) -> (x - x', y - y')) $ filterCoords (\(x, y) -> x >= x' - l && y >= y' - u && x <= x' + r && y <= y' + d) g
    ]

data Perimeter k = Perimeter
  { pTop :: [k],
    pRight :: [k],
    pBottom :: [k],
    pLeft :: [k]
  }
  deriving (Eq, Ord, Show)

pFrom :: Perimeter Coord2 -> Dir2 -> [Coord2]
pFrom p DirDown = pTop p
pFrom p DirLeft = pRight p
pFrom p DirRight = pLeft p
pFrom p DirUp = pBottom p

perimeterM :: (Griddable m g Coord2 a) => g Coord2 a -> m (Perimeter Coord2)
perimeterM g = do
  (maxX, maxY) <- maxXYM g
  let top = [(x, 0) | x <- [0 .. maxX]]
  let right = [(maxX, y) | y <- [0 .. maxY]]
  let bottom = [(x, maxY) | x <- [0 .. maxX]]
  let left = [(0, y) | y <- [0 .. maxY]]
  return $ Perimeter top right bottom left

perimeter :: (Griddable Identity g Coord2 a) => g Coord2 a -> Perimeter Coord2
perimeter = runIdentity . perimeterM

instance (Griddable Identity Grid' k a) => Memberable k (Grid' k a) where
  a ∈ g = gridMember a g

instance Unionable (Grid a) where
  (Grid a) ∪ (Grid b) = Grid (a ∪ b)

instance Intersectable (Grid a) where
  (Grid a) ∩ (Grid b) = Grid (a ∩ b)

instance (Griddable Identity Grid' k a) => Gettable Grid' k a where
  (|!) = (||!)

instance (Griddable Identity Grid' k a) => MaybeGettable Grid' k a where
  (|?) = (||?)

instance (Griddable Identity Grid' k a) => Settable Grid' k a where
  (|.) = (||.)

instance (Griddable Identity Grid' k a) => Modifiable Grid' k a where
  (|~) = (||~)

instance (Griddable Identity VectorGrid' k a) => Gettable VectorGrid' k a where
  (|!) = (||!)

instance (Griddable Identity VectorGrid' k a) => MaybeGettable VectorGrid' k a where
  (|?) = (||?)

instance (Griddable Identity VectorGrid' k a) => Settable VectorGrid' k a where
  (|.) = (||.)

instance (Griddable Identity VectorGrid' k a) => Modifiable VectorGrid' k a where
  (|~) = (||~)

instance (Griddable Identity VectorGrid' k a) => Memberable k (VectorGrid' k a) where
  a ∈ g = gridMember a g
