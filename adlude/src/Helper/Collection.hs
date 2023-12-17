module Helper.Collection where

import Data.Bimap qualified as BM
import Data.Char qualified as C
import Data.Foldable qualified as F
import Data.List qualified as L
import Data.List.Extra qualified as LE
import Data.List.Split qualified as LS
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Relude.Unsafe qualified as U

class Packable a b where
  pack :: a -> b
  unpack :: b -> a

instance Packable String T.Text where
  pack = T.pack
  unpack = T.unpack

splitOn :: String -> String -> [String]
splitOn = LS.splitOn

chunksOf :: Int -> [a] -> [[a]]
chunksOf = LE.chunksOf

minimum :: (Ord a) => [a] -> a
minimum = L.minimum

maximum :: (Ord a) => [a] -> a
maximum = L.maximum

nub :: (Eq a) => [a] -> [a]
nub = L.nub

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 = L.foldl1

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' = L.foldl1'

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 = L.foldr1

mkSet :: (Ord a) => [a] -> Set a
mkSet = S.fromList

unSet :: Set a -> [a]
unSet = S.toList

(<-|) :: (Ord a) => Set a -> a -> Set a
(<-|) = flip S.insert

(|->) :: (Ord a) => a -> Set a -> Set a
(|->) = S.insert

class Sizable a where
  size :: a -> Int

instance Sizable [a] where
  size = L.length

instance Sizable (Set a) where
  size = S.size

instance Sizable (Map k v) where
  size = M.size

instance Sizable (Seq a) where
  size = SQ.length

instance Sizable (PQ.MinPQueue k v) where
  size = PQ.size

class Memberable a b where
  (∈) :: a -> b -> Bool
  (∉) :: a -> b -> Bool
  a ∉ b = not $ a ∈ b
  (∋) :: b -> a -> Bool
  (∋) = flip (∈)
  (∌) :: b -> a -> Bool
  (∌) = flip (∉)

instance (Ord a) => Memberable a (Set a) where
  (∈) = S.member
  (∉) = S.notMember

instance (Eq a) => Memberable a [a] where
  (∈) = L.elem
  (∉) = L.notElem

instance (Ord a) => Memberable a (Map a b) where
  (∈) = M.member
  (∉) = M.notMember

class Unionable a where
  (∪) :: a -> a -> a

instance (Ord a) => Unionable (Set a) where
  (∪) = S.union

instance (Ord k) => Unionable (Map k v) where
  (∪) = M.union

instance Unionable [a] where
  (∪) = (L.++)

class Intersectable a where
  (∩) :: a -> a -> a

instance (Ord a) => Intersectable (Set a) where
  (∩) = S.intersection

instance (Ord k) => Intersectable (Map k v) where
  (∩) = M.intersection

instance (Eq a) => Intersectable [a] where
  (∩) = L.intersect

class Gettable f k v where
  (|!) :: f k v -> k -> v

instance (Ord k) => Gettable Map k v where
  (|!) = (M.!)

class MaybeGettable f k v where
  (|?) :: f k v -> k -> Maybe v

instance (Ord k) => MaybeGettable Map k v where
  (|?) = flip M.lookup

class Settable f k v where
  (|.) :: f k v -> (k, v) -> f k v

instance (Ord k) => Settable Map k v where
  m |. (k, v) = M.insert k v m

instance (Ord k) => Settable PQ.MinPQueue k v where
  m |. (k, v) = PQ.insert k v m

class Modifiable f k v where
  (|~) :: f k v -> (k, v -> v) -> f k v

instance (Ord k) => Modifiable Map k v where
  m |~ (k, f) = M.adjust f k m

(∅) :: Set a
(∅) = S.empty

(∖) :: (Ord a) => Set a -> Set a -> Set a
(∖) = S.difference

(⊆) :: (Ord a) => Set a -> Set a -> Bool
(⊆) = S.isSubsetOf

(⊈) :: (Ord a) => Set a -> Set a -> Bool
a ⊈ b = not (a ⊆ b)

(⊂) :: (Ord a) => Set a -> Set a -> Bool
(⊂) = S.isProperSubsetOf

(⊄) :: (Ord a) => Set a -> Set a -> Bool
a ⊄ b = not (a ⊂ b)

(⊇) :: (Ord a) => Set a -> Set a -> Bool
(⊇) = flip S.isSubsetOf

(⊉) :: (Ord a) => Set a -> Set a -> Bool
a ⊉ b = not (a ⊇ b)

(⊃) :: (Ord a) => Set a -> Set a -> Bool
(⊃) = flip S.isProperSubsetOf

(⊅) :: (Ord a) => Set a -> Set a -> Bool
a ⊅ b = not (a ⊃ b)

setMap :: (Ord b) => (a -> b) -> Set a -> Set b
setMap = S.map

mkMap :: (Ord k) => [(k, v)] -> Map k v
mkMap = M.fromList

unMap :: Map k v -> [(k, v)]
unMap = M.toList

mkMapWith :: (Ord k) => (v -> v -> v) -> [(k, v)] -> Map k v
mkMapWith = M.fromListWith

mapFilter :: (v -> Bool) -> Map k v -> Map k v
mapFilter = M.filter

mapSplit :: (Ord k) => k -> Map k v -> (Map k v, Map k v)
mapSplit = M.split

(|/) :: (Ord k) => Map k v -> k -> Map k v
m |/ k = M.delete k m

mkBimap :: (Ord a, Ord b) => [(a, b)] -> BM.Bimap a b
mkBimap = BM.fromList

mkSeq :: [a] -> Seq a
mkSeq = SQ.fromList

unSeq :: Seq a -> [a]
unSeq = F.toList

emptySeq :: Seq a
emptySeq = SQ.empty

(<|) :: a -> Seq a -> Seq a
(<|) = (SQ.<|)

(|>) :: Seq a -> a -> Seq a
(|>) = (SQ.|>)

(><) :: Seq a -> Seq a -> Seq a
(><) = (SQ.><)

(>/<) :: (Eq a) => Seq a -> a -> Seq a
s >/< a = foldl' (flip SQ.deleteAt) s (SQ.elemIndicesL a s)

mkMinQ :: (Ord k) => [(k, a)] -> PQ.MinPQueue k a
mkMinQ = PQ.fromList

nullQ :: PQ.MinPQueue k a -> Bool
nullQ = PQ.null

(<!) :: (Ord k) => PQ.MinPQueue k a -> ((k, a), PQ.MinPQueue k a)
(<!) q = PQ.deleteFindMin q

(!!) :: [a] -> Int -> a
(!!) = (U.!!)

uhead :: [a] -> a
uhead = U.head

utail :: [a] -> [a]
utail = U.tail

uinit :: [a] -> [a]
uinit = U.init

ulast :: [a] -> a
ulast = U.last

uread :: (Read a) => String -> a
uread = U.read

digitToInt :: Char -> Int
digitToInt = C.digitToInt

intToDigit :: Int -> Char
intToDigit = C.intToDigit
