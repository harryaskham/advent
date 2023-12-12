module Helper.Collection where

import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Relude.Unsafe qualified as U
import Data.List qualified as L
import Data.Foldable qualified as F
import Data.List.Split qualified as LS
import Data.List.Extra qualified as LE
import Data.Text qualified as T
import Data.Char qualified as C

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

nub :: Eq a => [a] -> [a]
nub = L.nub

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 = L.foldl1

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' = L.foldl1'

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 = L.foldr1

mkSet :: (Ord a) => [a] -> Set a
mkSet = S.fromList

unSet :: (Ord a) => Set a -> [a]
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

(∅) :: (Ord a) => Set a
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

setSize :: (Ord a) => Set a -> Int
setSize = S.size

mkMap :: (Ord k) => [(k, v)] -> Map k v
mkMap = M.fromList

unMap :: (Ord k) => Map k v -> [(k, v)]
unMap = M.toList

mkMapWith :: (Ord k) => (v -> v -> v) -> [(k, v)] -> Map k v
mkMapWith = M.fromListWith

unionWith :: (Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith = M.unionWith

mapFilter :: (Ord k) => (v -> Bool) -> Map k v -> Map k v
mapFilter = M.filter

mapSize :: (Ord k) => Map k v -> Int
mapSize = M.size

keys :: (Ord k) => Map k v -> [k]
keys = M.keys

(|!) :: (Ord k) => Map k v -> k -> v
(|!) = (M.!)

(|?) :: (Ord k) => Map k v -> k -> Maybe v
(|?) = flip M.lookup

adjust :: (Ord k) => (v -> v) -> k -> Map k v -> Map k v
adjust = M.adjust

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
