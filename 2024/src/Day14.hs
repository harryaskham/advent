module Day14 (part1, part2) where

import Data.Text qualified as T
import System.Console.ANSI
import System.IO (stdout)

robots :: (ℤ², [ℤ⁴])
robots = ((101, 103), $(aoc 14) |-. tuples @4 (numbers @ℤ))

xrobots :: (ℤ², [ℤ⁴])
xrobots = ((11, 7), $(aocx 14) |-. tuples @4 (numbers @ℤ))

step :: ℤ² -> ℤ⁴ -> ℤ⁴
step (w, h) (x, y, vx, vy) = ((x + vx) `mod` w, (y + vy) `mod` h, vx, vy)

rev :: ℤ² -> ℤ⁴ -> ℤ⁴
rev (w, h) (x, y, vx, vy) = ((x - vx) `mod` w, (y - vy) `mod` h, vx, vy)

part1 :: ℤ
part1 =
  let (dims@(w, h), rs) = robots
      (xmid, ymid) = both (`div` 2) dims
      moved = foldl' (\rs _ -> step dims <$> rs) rs [1 .. 100]
      xys = traceShowId $ (\(x, y, _, _) -> (x, y)) <$> moved
      nonmids = [(x, y) | (x, y) <- xys, x ≢ xmid, y ≢ ymid]
      (ul, ur, dl, dr) =
        foldl'
          ( \(ul, ur, dl, dr) p@(x, y) ->
              case (x < xmid, y < ymid) of
                (True, True) -> (p : ul, ur, dl, dr)
                (True, False) -> (ul, ur, p : dl, dr)
                (False, True) -> (ul, p : ur, dl, dr)
                (False, False) -> (ul, ur, dl, p : dr)
          )
          ([], [], [], [])
          nonmids
   in product (size <$> [ul, ur, dl, dr])

showRobots :: ℤ² -> [ℤ⁴] -> Text
showRobots (w, h) rs =
  let (g :: G ℤ² ℕ₁₀) = mkGrid [((x, y), (0 :: ℕ₁₀)) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
      g' = foldl' (\g (x, y, _, _) -> g ||~ ((x, y), (+ 1))) g rs
   in T.replace "0" " " (pretty g')

findTree :: ℤ -> ℤ² -> [ℤ⁴] -> IO ℤ
findTree n dims rs = do
  clearScreen
  putTextLn $ "step " <> tshow n
  let s = showRobots dims rs
  if "1111111111111111111111111111111" `T.isInfixOf` s
    then do
      putTextLn s
      c <- getLine
      case T.unpack c of
        "p" -> findTree (n - 1) dims (rev dims <$> rs)
        ('p' : ns) -> let n' = ns |- number in findTree (n - n') dims (foldl' (\rs _ -> rev dims <$> rs) rs [0 .. n' - 1])
        ('f' : ns) -> let n' = ns |- number in findTree (n + n') dims (foldl' (\rs _ -> step dims <$> rs) rs [0 .. n' - 1])
        _ -> findTree (n + 1) dims (step dims <$> rs)
    else findTree (n + 1) dims (step dims <$> rs)

part2 :: IO ℤ
part2 = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  let (dims, rs) = robots
  findTree 0 dims rs
