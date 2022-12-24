module Day24 (part1, part2) where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Set qualified as S
import Data.Vector (Vector)
import Data.Vector qualified as V
import Helper.Coord (Coord2, Dir2 (..), manhattan, move, neighborsNoDiags)
import Helper.Grid (Grid, GridCell (charMap), maxXY, readGrid)
import Helper.TH (input)

data Cell
  = Empty
  | Wall
  | Blizzard [Dir2]
  deriving (Eq, Ord)

instance GridCell Cell where
  charMap =
    BM.fromList
      [ (Empty, '.'),
        (Wall, '#'),
        (Blizzard [DirUp], '^'),
        (Blizzard [DirDown], 'v'),
        (Blizzard [DirLeft], '<'),
        (Blizzard [DirRight], '>')
      ]

stepBlizzards :: Coord2 -> Grid Cell -> Grid Cell
stepBlizzards (w, h) g =
  let wrap (x, y) =
        let x'
              | x == 0 = w - 1
              | x == w = 1
              | otherwise = x
            y'
              | y == 0 = h - 1
              | y == h = 1
              | otherwise = y
         in (x', y')
      isBlizzard (Blizzard _) = True
      isBlizzard _ = False
      blizzards =
        Blizzard
          <$> M.fromListWith
            (++)
            [ (wrap (move d 1 (x, y)), [d])
              | ((x, y), v) <- M.toList g,
                isBlizzard v,
                let Blizzard ds = v,
                d <- ds
            ]
      noBlizzard (Blizzard _) = Empty
      noBlizzard a = a
   in M.union blizzards (noBlizzard <$> g)

shortestPath :: Vector (Grid Cell) -> Coord2 -> Coord2 -> Maybe Int
shortestPath gs start end = go (PQ.singleton (h startState) startState) S.empty
  where
    h (c, n) = n + manhattan c end
    startState = (start, 0)
    go queue seen
      | PQ.null queue = Nothing
      | current == end = Just n
      | cacheKey `S.member` seen = go rest seen
      | otherwise = go queue' seen'
      where
        ((_, (current, n)), rest) = PQ.deleteFindMin queue
        cacheKey = (current, n `mod` length gs)
        g = gs V.! ((n + 1) `mod` length gs)
        seen' = S.insert cacheKey seen
        next = [(c, n + 1) | c <- neighborsNoDiags current, M.lookup c g == Just Empty]
        nextWithWait = if g M.! current == Empty then (current, n + 1) : next else next
        queue' = foldl' (\q st -> PQ.insert (h st) st q) rest nextWithWait

part1 :: Maybe Int
part1 =
  let g = readGrid $(input 24)
      (w, h) = maxXY g
      period = lcm (w - 1) (h - 1)
      gs = V.fromList (take period (iterate (stepBlizzards (w, h)) g))
   in shortestPath gs (1, 0) (w - 1, h)

part2 :: Maybe Int
part2 =
  let g = readGrid $(input 24)
      (w, h) = maxXY g
      period = lcm (w - 1) (h - 1)
      gs = cycle (take period (iterate (stepBlizzards (w, h)) g))
      start = (1, 0)
      end = (w - 1, h)
   in do
        a <- shortestPath (V.fromList (take period gs)) start end
        b <- shortestPath (V.fromList (take period (drop a gs))) end start
        c <- shortestPath (V.fromList (take period (drop (a + b) gs))) start end
        return $ a + b + c
