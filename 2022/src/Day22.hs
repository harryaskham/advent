module Day22 (part1, part2) where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Helper.Coord (Coord2, Dir2 (..), move, rlToTurn)
import Helper.Grid (Grid, GridCell (charMap), fillDef, maxXY, readGrid)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec (Parser, char, eof, many1, (<|>))
import Prelude hiding ((<|>))

data Cell = OOB | Open | Solid | Current deriving (Eq, Ord)

instance GridCell Cell where
  charMap =
    BM.fromList
      [ (OOB, ' '),
        (Open, '.'),
        (Solid, '#')
      ]

data Move = MoveFwd Int | Rotate (Dir2 -> Dir2)

parser :: Parser [Move]
parser = many1 move <* (eol >> eof)
  where
    move = moveFwd <|> rotate
    moveFwd = MoveFwd <$> number
    rotate = Rotate . rlToTurn <$> (char 'L' <|> char 'R')

type Wrapper = Grid Cell -> Coord2 -> Coord2 -> Dir2 -> (Coord2, Dir2)

linearWrap :: Wrapper
linearWrap g (w, h) (x, y) f = (,f) . firstCoord $ case f of
  DirRight -> (,y) <$> [0 ..]
  DirLeft -> (,y) <$> [w, w - 1 ..]
  DirDown -> (x,) <$> [0 ..]
  DirUp -> (x,) <$> [h, h - 1 ..]
  where
    firstCoord cs = U.head [c | c <- cs, g M.! c /= OOB]

cubicWrap :: Wrapper
cubicWrap _ (w, h) (x, y) f
  | f == DirUp && y == -1 && x >= 50 && x < 100 = ((0, 150 + (x - 50)), DirRight)
  | f == DirLeft && x == -1 && y >= 150 && y < 200 = ((50 + (y - 150), 0), DirDown)
  | f == DirUp && y == -1 && x >= 100 && x < 150 = ((x - 100, h), DirUp)
  | f == DirDown && y == h + 1 && x >= 0 && x < 50 = ((x + 100, 0), DirDown)
  | f == DirRight && x == w + 1 && y >= 0 && y < 50 = ((99, 149 - y), DirLeft)
  | f == DirRight && x == 100 && y >= 100 && y < 150 = ((w, 149 - y), DirLeft)
  | f == DirDown && y == 50 && x >= 100 && x < 150 = ((99, 50 + (x - 100)), DirLeft)
  | f == DirRight && x == 100 && y >= 50 && y < 100 = ((100 + (y - 50), 49), DirUp)
  | f == DirDown && y == 150 && x >= 50 && x < 100 = ((49, 150 + (x - 50)), DirLeft)
  | f == DirRight && x == 50 && y >= 150 && y < 200 = ((50 + (y - 150), 149), DirUp)
  | f == DirLeft && x == -1 && y >= 100 && y < 150 = ((50, 149 - y), DirRight)
  | f == DirLeft && x == 49 && y >= 0 && y < 50 = ((0, 149 - y), DirRight)
  | f == DirUp && y == 99 && x >= 0 && x < 50 = ((50, 50 + x), DirRight)
  | f == DirLeft && x == 49 && y >= 50 && x < 100 = ((y - 50, 100), DirDown)

fScore :: Dir2 -> Int
fScore DirRight = 0
fScore DirDown = 1
fScore DirLeft = 2
fScore DirUp = 3

followPath :: Wrapper -> Grid Cell -> [Move] -> Int
followPath wrap g = go (50, 0) DirRight
  where
    (w, h) = maxXY g
    go (x, y) f [] = sum [4 * (x + 1), 1000 * (y + 1), fScore f]
    go (x, y) f ((Rotate r) : path) = go (x, y) (r f) path
    go (x, y) f ((MoveFwd 0) : path) = go (x, y) f path
    go (x, y) f ((MoveFwd n) : path)
      | g M.! next == Solid = go (x, y) f path
      | otherwise = go next nextF (MoveFwd (n - 1) : path)
      where
        nextC = move f 1 (x, y)
        (next, nextF) =
          if M.lookup nextC g `elem` [Nothing, Just OOB]
            then wrap g (w, h) nextC f
            else (nextC, f)

solve :: Wrapper -> Int
solve wrap =
  $(input 22)
    & lines
    & (fillDef OOB . readGrid . unlines . take 200)
      &&& (parseWith parser . T.unpack . unlines . drop 201)
    & uncurry (followPath wrap)

part1 :: Int
part1 = solve linearWrap

part2 :: Int
part2 = solve cubicWrap
