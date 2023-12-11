module Helper.Collection where

import Data.Map.Strict qualified as M
import Data.Set qualified as S

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