{-# LANGUAGE TupleSections #-}

module TwentyEighteen.Day20 where

import Coord (Coord2, Dir2, move, nsewToDir2)
import Data.List (scanl')
import Data.List.Extra (replace)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra (second, swap)
import Grid (maxXY, minXY, pretty)
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    eof,
    many,
    many1,
    oneOf,
    sepBy,
    (<|>),
  )
import Util (eol, input, readWithParser)

data Path = SimplePath [Dir2] | BranchingPath [[Path]] | DeadEnd deriving (Show)

regex :: GenParser Char () [Path]
regex = do
  char '^'
  p <- path
  char '$'
  eol
  eof
  return p
  where
    path :: GenParser Char () [Path]
    path = many (simplePath <|> branchingPath)
    simplePath :: GenParser Char () Path
    simplePath = do
      dirs <- many1 $ oneOf "NSEW_"
      return $ case dirs of
        "_" -> DeadEnd
        nsew -> SimplePath $ nsewToDir2 <$> nsew
    branchingPath :: GenParser Char () Path
    branchingPath = do
      char '('
      paths <- path `sepBy` char '|'
      char ')'
      return $ BranchingPath paths

bfsConnectivity :: Seq (Coord2, [Path]) -> Set (Coord2, Coord2) -> Set (Coord2, Coord2)
bfsConnectivity SQ.Empty connected = connected
bfsConnectivity ((_, []) SQ.:<| queue) connected = bfsConnectivity queue connected
bfsConnectivity ((pos, path : paths) SQ.:<| queue) connected =
  case path of
    SimplePath dirs ->
      let pathPos = scanl' (\pos d -> move d 1 pos) pos dirs
          newConnections = S.fromList $ zip pathPos (drop 1 pathPos)
          newConnected = connected `S.union` newConnections
          newPos = last pathPos
       in bfsConnectivity (queue SQ.|> (newPos, paths)) newConnected
    BranchingPath bPaths ->
      let bfsOnPath bp = bfsConnectivity (SQ.singleton (pos, bp ++ paths)) connected
       in bfsConnectivity queue (foldl1 S.union $ bfsOnPath <$> bPaths)
    DeadEnd -> bfsConnectivity queue connected

longestPath :: Map Coord2 [Coord2] -> Seq (Coord2, Int, Set Coord2) -> Int -> Int
longestPath _ SQ.Empty maxN = maxN
longestPath connectivity ((pos, n, seen) SQ.:<| queue) maxN
  | pos `S.member` seen = longestPath connectivity queue maxN
  | otherwise = longestPath connectivity (queue SQ.>< next) (max n maxN)
  where
    next = SQ.fromList $ case M.lookup pos connectivity of
      Nothing -> []
      Just nextPos -> (,n + 1,S.insert pos seen) <$> nextPos

accessibleInOverN :: Map Coord2 [Coord2] -> Int -> Seq (Coord2, Int, Set Coord2) -> Set Coord2 -> Int
accessibleInOverN _ _ SQ.Empty allSeen = S.size allSeen
accessibleInOverN connectivity maxLen ((pos, n, seen) SQ.:<| queue) allSeen
  | pos `S.member` seen = accessibleInOverN connectivity maxLen queue allSeen
  | n >= maxLen = accessibleInOverN connectivity maxLen (queue SQ.>< next) (S.insert pos allSeen)
  | otherwise = accessibleInOverN connectivity maxLen (queue SQ.>< next) allSeen
  where
    next = SQ.fromList $ case M.lookup pos connectivity of
      Nothing -> []
      Just nextPos -> (,n + 1,S.insert pos seen) <$> nextPos

readConnectivity :: IO (Map Coord2 [Coord2])
readConnectivity = do
  ps <- readWithParser regex . replace "|)" "|_)" <$> input 2018 20
  let connected = bfsConnectivity (SQ.singleton ((0, 0), ps)) S.empty
  return
    . M.fromListWith (++)
    . fmap (second pure)
    $ S.toList (connected `S.union` S.map swap connected)

part1 :: IO Int
part1 = do
  connectivity <- readConnectivity
  return $ longestPath connectivity (SQ.singleton ((0, 0), 0, S.empty)) 0

part2 :: IO Int
part2 = do
  connectivity <- readConnectivity
  return $ accessibleInOverN connectivity 1000 (SQ.singleton ((0, 0), 0, S.empty)) S.empty

-- Below here, convenience functions for prettyprinting the connectivity map

data Cell = Wall | Empty | HDoor | VDoor

instance Show Cell where
  show Wall = "#"
  show Empty = "."
  show HDoor = "|"
  show VDoor = "-"

prettyConnectivity :: Map Coord2 [Coord2] -> String
prettyConnectivity connectivity = pretty g
  where
    accessible = S.fromList $ M.keys connectivity
    (minX, minY) = minXY connectivity
    (maxX, maxY) = maxXY connectivity
    offsetX = 2 * minX - 1
    offsetY = 2 * minY - 1
    grid =
      [ ( (2 * x - offsetX, 2 * y - offsetY),
          if (x, y) `S.member` accessible
            then Empty
            else Wall
        )
        | x <- [minX .. maxX],
          y <- [minY .. maxY]
      ]
    hDoors =
      [ ( (2 * x + 1 - offsetX, 2 * y - offsetY),
          if (x + 1, y) `M.member` connectivity
            && (x, y) `M.member` connectivity
            && (elem (x + 1, y) <$> M.lookup (x, y) connectivity) == Just True
            then HDoor
            else Wall
        )
        | x <- [minX .. maxX],
          y <- [minY .. maxY]
      ]
    vDoors =
      [ ( (2 * x - offsetX, 2 * y + 1 - offsetY),
          if (x, y + 1) `M.member` connectivity
            && (x, y) `M.member` connectivity
            && ((elem (x, y + 1)) <$> M.lookup (x, y) connectivity) == Just True
            then VDoor
            else Wall
        )
        | x <- [minX .. maxX],
          y <- [minY .. maxY]
      ]
    allWalls =
      [ ((x - offsetX, y - offsetY), Wall)
        | x <- [2 * minX - 1 .. 2 * maxX + 1],
          y <- [2 * minX - 1 .. 2 * maxX + 1]
      ]
    g = M.fromListWith (flip const) $ grid ++ hDoors ++ vDoors ++ allWalls
