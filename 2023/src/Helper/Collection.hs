module Helper.Collection where

import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Relude.Unsafe qualified as U

mkSet :: (Ord a) => [a] -> Set a
mkSet = S.fromList

(<-|) :: (Ord a) => Set a -> a -> Set a
(<-|) = flip S.insert

(|->) :: (Ord a) => a -> Set a -> Set a
(|->) = S.insert

mkMap :: (Ord k) => [(k, v)] -> Map k v
mkMap = M.fromList

(|!) :: (Ord k) => Map k v -> k -> v
(|!) = (M.!)

(|?) :: (Ord k) => Map k v -> k -> Maybe v
(|?) = flip M.lookup

adjust :: (Ord k) => (v -> v) -> k -> Map k v -> Map k v
adjust = M.adjust

type Bimap = BM.Bimap

mkBimap :: (Ord a, Ord b) => [(a, b)] -> Bimap a b
mkBimap = BM.fromList

mkSeq :: [a] -> Seq a
mkSeq = SQ.fromList

mkMinQ :: (Ord k) => [(k, a)] -> PQ.MinPQueue k a
mkMinQ = PQ.fromList

(!!) :: [a] -> Int -> a
(!!) = (U.!!)

head :: [a] -> a
head = U.head

tail :: [a] -> [a]
tail = U.tail

init :: [a] -> [a]
init = U.init

last :: [a] -> a
last = U.last