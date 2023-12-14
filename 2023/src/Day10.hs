module Day10 (part1, part2) where

data Cell = None | Start | PipeVertical | PipeHorizontal | PipeF | Pipe7 | PipeL | PipeJ deriving (Eq, Ord, Show)

instance GridCell Cell where
  charMap = mkBimap [(None, '.'), (Start, 'S'), (PipeVertical, '|'), (PipeHorizontal, '-'), (PipeF, 'F'), (Pipe7, '7'), (PipeL, 'L'), (PipeJ, 'J')]

pathOutside :: Grid Cell -> (([Coord2], Set Coord2), ([Coord2], Set Coord2))
pathOutside g =
  let pipesTo n@(x, y) c =
        c `elem` case g ||? n of
          Just PipeVertical -> [(x, y + 1), (x, y - 1)]
          Just PipeHorizontal -> [(x + 1, y), (x - 1, y)]
          Just PipeF -> [(x + 1, y), (x, y + 1)]
          Just Pipe7 -> [(x - 1, y), (x, y + 1)]
          Just PipeL -> [(x + 1, y), (x, y - 1)]
          Just PipeJ -> [(x - 1, y), (x, y - 1)]
          _ -> []
      outsideOf (x, y) (lx, ly) =
        case ((x - lx, y - ly), g ||? (x, y)) of
          ((0, 1), Just PipeVertical) -> [(x + 1, y)]
          ((0, -1), Just PipeVertical) -> [(x - 1, y)]
          ((1, 0), Just PipeHorizontal) -> [(x, y - 1)]
          ((-1, 0), Just PipeHorizontal) -> [(x, y + 1)]
          ((0, -1), Just PipeF) -> [(x - 1, y), (x, y - 1)]
          ((-1, 0), Just PipeF) -> [(x + 1, y + 1)]
          ((1, 0), Just Pipe7) -> [(x + 1, y), (x, y - 1)]
          ((0, -1), Just Pipe7) -> [(x - 1, y + 1)]
          ((-1, 0), Just PipeL) -> [(x, y + 1), (x - 1, y)]
          ((0, 1), Just PipeL) -> [(x + 1, y - 1)]
          ((1, 0), Just PipeJ) -> [(x - 1, y - 1)]
          ((0, 1), Just PipeJ) -> [(x + 1, y), (x, y + 1)]
          _ -> []
      start = gridFindOne Start g
      go path@(c : _) outside l reverse =
        let go' n = go (n : path) (foldl' (<-|) outside (outsideOf n c)) (Just c) reverse
         in case [n | n <- neighborsNoDiags c, Just n /= l, n `pipesTo` c, c == start || c `pipesTo` n] of
              [] -> (path, outside)
              (n : m : _) -> if reverse then go' m else go' n
              (n : _) -> go' n
   in both (go [start] (∅) Nothing) (False, True)

enclosed :: Grid Cell -> Set Coord2 -> Set Coord2 -> Set Coord2
enclosed g path outside =
  let go seen Empty = (seen, seen)
      go seen (c :<| cs)
        | c ∈ (seen <> path) = go seen cs
        | c ∈ outside = (seen, (∅))
        | otherwise =
            let ns = [n | n <- neighborsNoDiags c, n ∉ path, n ||∈ g]
             in go (c |-> seen) (cs >< mkSeq ns)
      goAll _ es [] = es
      goAll seen es (start : starts)
        | start ∈ seen = goAll seen es starts
        | otherwise =
            let (seen', es') = go (∅) (mkSeq [start])
             in goAll (seen ∪ seen') (es ∪ es') starts
   in goAll (∅) (∅) [c | c <- coords g, c ∉ path]

part1 :: Int
part1 = $(grid input 10) & (pathOutside >>> fst . fst >>> length >>> (+ 1) >>> (`div` 2))

part2 :: Int
part2 =
  $(grid input 10)
    & ((enclosed >>> uncurry) &&& (pathOutside >>> both (first mkSet)))
    & uncurry both
    & both (bool id (const (∅)) . ((0, 0) ∈) &&& id >>> uncurry ($))
    & uncurry (∪)
    & setSize