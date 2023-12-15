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

(∈) :: (Ord a) => a -> Set a -> Bool
(∈) = S.member

(|∈) :: (Ord a) => a -> Map a b -> Bool
(|∈) = M.member

(∋) :: (Ord a) => Set a -> a -> Bool
(∋) = flip S.member

(∉) :: (Ord a) => a -> Set a -> Bool
(∉) = S.notMember

(∌) :: (Ord a) => Set a -> a -> Bool
(∌) = flip S.notMember

(∅) :: Set a
(∅) = S.empty

(∪) :: (Ord a) => Set a -> Set a -> Set a
(∪) = S.union

(∩) :: (Ord a) => Set a -> Set a -> Set a
(∩) = S.intersection

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

setSize :: Set a -> Int
setSize = S.size

mkMap :: (Ord k) => [(k, v)] -> Map k v
mkMap = M.fromList

unMap :: Map k v -> [(k, v)]
unMap = M.toList

mkMapWith :: (Ord k) => (v -> v -> v) -> [(k, v)] -> Map k v
mkMapWith = M.fromListWith

mapFilter :: (v -> Bool) -> Map k v -> Map k v
mapFilter = M.filter

mapSize :: Map k v -> Int
mapSize = M.size

mapSplit :: (Ord k) => k -> Map k v -> (Map k v, Map k v)
mapSplit = M.split

(|!) :: (Ord k) => Map k v -> k -> v
(|!) = (M.!)

(|?) :: (Ord k) => Map k v -> k -> Maybe v
(|?) = flip M.lookup

(|.) :: (Ord k) => Map k v -> (k, v) -> Map k v
m |. (k, v) = M.insert k v m

(|~) :: (Ord k) => Map k v -> (k, v -> v) -> Map k v
m |~ (k, f) = M.adjust f k m

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
