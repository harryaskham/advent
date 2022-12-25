module Day25 (part1, part2, snafuToInt, intToSnafu) where

import Data.Text qualified as T
import Helper.TH (input)
import Relude.Unsafe qualified as U

snafuToInt :: String -> Int
snafuToInt s = go (reverse s) 0 0
  where
    go [] _ a = a
    go (c : cs) n a = go cs (n + 1) (a + snafuVal c * 5 ^ n)
    snafuVal '0' = 0
    snafuVal '1' = 1
    snafuVal '2' = 2
    snafuVal '-' = -1
    snafuVal '=' = -2

intToSnafu :: Int -> String
intToSnafu a = make a powers ""
  where
    n = U.head [length s | s <- scanl' (++) "" (repeat "2"), let a' = snafuToInt s, a' >= a]
    powers = reverse . take n $ scanl1 (*) (1 : repeat 5)
    make _ [] s = reverse s
    make x (p : ps) s
      | x < 0 && p + 2 * sum ps < abs x = make (x + 2 * p) ps ('=' : s)
      | x < 0 && 2 * sum ps < abs x = make (x + 1 * p) ps ('-' : s)
      | x > 0 && p + 2 * sum ps < x = make (x - 2 * p) ps ('2' : s)
      | x > 0 && 2 * sum ps < x = make (x - 1 * p) ps ('1' : s)
      | otherwise = make x ps ('0' : s)

part1 :: String
part1 =
  $(input 25)
    & lines
    & fmap (snafuToInt . T.unpack)
    & sum
    & intToSnafu

part2 :: Text
part2 = "Merry Christmas!"
