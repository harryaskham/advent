module TwentyTwenty.Day23 where

import Data.Char (digitToInt, intToDigit)
import qualified Data.Foldable as F
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq ((:<|)), (<|), (><), (|>))
import qualified Data.Sequence as SQ
import Data.Tuple.Extra
import Debug.Trace

input :: [Int]
input = digitToInt <$> "389125467"

--input = digitToInt <$> "496138527"
{-
move :: [Int] -> [Int]
move (c : cs) = take 9 . drop 1 $ cycle nextCups
  where
    remove = take 3 cs
    rest = c : drop 3 cs
    destinations' = [c - 1, c - 2, c - 3, c - 4]
    destinations = (\d -> if d <= 0 then d + 9 else d) <$> destinations'
    destination = head $ filter (not . (`elem` remove)) destinations
    [a, b] = splitOn [destination] rest
    nextCups = a ++ [destination] ++ remove ++ b
-}

-- maintain a parallel sequence
-- where the item'th element is its index in the other one
-- need a map from elem to index
-- after each go, things before destination unchanged
-- each moved thing gets destination +1,2,3,
-- things after destination get destination+4,5,6

-- with 389125467
-- ixs  340657812
-- current is ix0, elem 3
-- dest is 2, look up in ixs, get index 4
-- split as 389125467
--          340657812
-- give     328915467, current 2
--          410657823
-- give     325467891, current 5
--          810324567
-- the 1 (telling us where 8, the first remover, is, has migrated back to the destination position)
-- so shift back 1, then

-- if it was a map, we'd have up to destination things unchanged
-- everything after initial, and before / including destination, gets -3 index

makeIxs :: [Int] -> M.Map Int Int
makeIxs xs = M.fromList $ zip xs [0 ..]

-- fast find element (implies hashing)
-- fast remove / insert
-- exploit radix property
-- has to be something about storing the reverse indices
-- or repeating positions?

type State = (Int, M.Map Int Int, SQ.Seq Int)

move :: State -> State
move (step, ixs, cups@(c :<| r1 :<| r2 :<| r3 :<| rest)) =
  --trace (show (step, cups, c, [r1, r2, r3], rest, (d, dIx), a, b, snd <$> sort (M.toList ixs))) $
  trace (show step) $
    (step + 1, nextIxs, nextCups)
  where
    ds' = [c - 1, c - 2, c - 3, c - 4]
    ds = (\d -> if d <= 0 then d + length cups else d) <$> ds'
    d = head $ filter (not . (`elem` [r1, r2, r3])) ds
    --Just dIx = SQ.elemIndexL d rest
    --dIx = 1000
    -- dIx = ixs M.! d - step
    dIx = ixs M.! d
    (_ :<| _ :<| _ :<| _ :<| a, b) = SQ.splitAt (dIx + 1) cups
    --(a, b) = SQ.splitAt (dIx + 1) rest
    nextCups = a >< ((r1 <| r2 <| r3 <| b) |> c)
    ixUpdates =
      [(c, const $ length cups)]
        ++ [(cup, const i) | (i, cup) <- zip [0 ..] (F.toList a)]
        ++ [(r, const (length a + i)) | (i, r) <- zip [0 ..] [r1, r2, r3]]
        ++ [(cup, const (length a + 3 + i)) | (i, cup) <- zip [0 ..] (F.toList b)]
    nextIxs = foldl' (\acc (k, f) -> M.adjust f k acc) ixs ixUpdates

alignTo1 :: SQ.Seq Int -> SQ.Seq Int
alignTo1 cups@(c :<| cs)
  | c == 1 = cups
  | otherwise = alignTo1 (cs |> c)

part1 :: Int
part1 =
  read
    . drop 1
    . fmap intToDigit
    . F.toList
    . alignTo1
    . thd3
    $ (iterate move (1, makeIxs input, SQ.fromList input)) !! 100

longInput :: [Int]
longInput = input ++ [10 .. 1000000]

part2 :: Int
part2 =
  product
    . take 2
    . drop 1
    . F.toList
    . alignTo1
    . thd3
    $ (iterate move (0, makeIxs longInput, SQ.fromList longInput)) !! 10000000
