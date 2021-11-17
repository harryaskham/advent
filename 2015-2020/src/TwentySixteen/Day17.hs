module TwentySixteen.Day17 where

import Coord (Coord2)
import qualified Data.Sequence as SQ
import Util (md5String)

passcode :: String
passcode = "yjjvjgan"

nextStates :: Coord2 -> String -> [(Coord2, String)]
nextStates (x, y) path =
  (\(pos, c, _) -> (pos, c : path))
    <$> ( filter
            (\((x, y), _, open) -> open && x >= 0 && x <= 3 && y >= 0 && y <= 3)
            [ ((x + 1, y), 'R', doorOpen 3),
              ((x - 1, y), 'L', doorOpen 2),
              ((x, y + 1), 'D', doorOpen 1),
              ((x, y -1), 'U', doorOpen 0)
            ]
        )
  where
    doors = take 4 $ md5String (passcode ++ reverse path)
    doorOpen i = doors !! i `elem` "bcdef"

shortestPath :: String
shortestPath = go (SQ.singleton ((0, 0), []))
  where
    go SQ.Empty = "No path"
    go ((pos, path) SQ.:<| queue)
      | pos == (3, 3) = reverse path
      | otherwise = go $ queue SQ.>< SQ.fromList (nextStates pos path)

part1 :: String
part1 = shortestPath

longestPath :: Int
longestPath = go (SQ.singleton ((0, 0), [])) 0
  where
    go SQ.Empty best = best
    go ((pos, path) SQ.:<| queue) best
      | pos == (3, 3) = go queue (maximum [length path, best])
      | otherwise = go (queue SQ.>< SQ.fromList (nextStates pos path)) best

part2 :: Int
part2 = longestPath
