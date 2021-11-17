{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module TwentyNineteen.Day6 where

import Control.Monad (join)
import qualified Data.List.Safe as LS
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Util (input)

data OrbitTree = OrbitTree String [OrbitTree] deriving (Show)

orbitMap :: [String] -> M.Map String [String]
orbitMap = foldl addEntry M.empty
  where
    addEntry acc orbit = M.insertWith (++) oTo [oFrom] acc
      where
        [oTo, oFrom] = splitOn ")" orbit

toTree :: M.Map String [String] -> OrbitTree
toTree om = go "COM"
  where
    go :: String -> OrbitTree
    go n = OrbitTree n (go <$> childStrings)
      where
        childStrings = fromMaybe [] $ M.lookup n om

countOrbits :: OrbitTree -> Int
countOrbits = go 0
  where
    go depth (OrbitTree _ []) = depth
    go depth (OrbitTree _ cs) = depth + sum (go (depth + 1) <$> cs)

santaDistance :: OrbitTree -> Maybe Int
santaDistance ot@(OrbitTree _ cs) =
  (-)
    <$> (LS.minimum =<< sequenceA (filter isJust (distanceFromHere : distanceFromChildren)))
    <*> Just 2
  where
    distanceTo depth target (OrbitTree n []) =
      if n == target then Just depth else Nothing
    distanceTo depth target (OrbitTree n cs) =
      if n == target then Just depth else join $ LS.head $ filter isJust children
      where
        children = distanceTo (depth + 1) target <$> cs
    distanceFromHere = (+) <$> distanceTo 0 "SAN" ot <*> distanceTo 0 "YOU" ot
    distanceFromChildren = santaDistance <$> cs

part12 :: IO (Int, Maybe Int)
part12 = do
  orbits <- lines <$> input 2019 6
  let tree = toTree . orbitMap $ orbits
  return (countOrbits tree, santaDistance tree)
