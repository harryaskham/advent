module TwentyTwenty.Day23 where

import Data.Char (digitToInt, intToDigit)
import qualified Data.Foldable as F
import Data.IORef
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq ((:<|)), (<|), (><), (|>))
import qualified Data.Sequence as SQ
import Data.Tuple.Extra
import Debug.Trace

input :: [Int]
--input = digitToInt <$> "389125467"
input = digitToInt <$> "496138527"

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

data Ring a = Ring (Seq a) (M.Map a Int) deriving (Show)

mkRing :: [Int] -> Ring Int
mkRing xs = Ring (SQ.fromList xs) (makeIxs xs)

ringToSeq :: Ring a -> Seq a
ringToSeq (Ring s _) = s

get :: (Ord a) => Int -> Ring a -> a
get ix (Ring s _) = let Just x = SQ.lookup (ix `mod` (length s)) s in x

ixOf :: (Ord a, Eq a) => a -> Ring a -> Int
ixOf a (Ring _ ixs) = ixs M.! a

insertNAt :: Ord a => Int -> Seq a -> Ring a -> Ring a
insertNAt ix xs (Ring s ixs) = Ring newS newIxs
  where
    (a, b) = SQ.splitAt ix s
    newS = a >< xs >< b
    -- Slow
    newIxs' = (\v -> if v >= ix then v + (length xs) else v) <$> ixs
    newIxs = foldl' (\acc (i, x) -> M.insert x (ix + i) acc) newIxs' (zip [0 ..] (F.toList xs))

deleteAt :: (Ord a) => Int -> Ring a -> (Ring a, a)
deleteAt ix' (Ring s ixs) = (Ring newS newIxs, removed)
  where
    ix = if ix' >= length s then 0 else ix'
    (a, removed :<| b) = SQ.splitAt ix s
    newS = a >< b
    -- Slow
    newIxs' = (\v -> if v > ix then v - 1 else v) <$> ixs
    newIxs = M.delete removed newIxs'

-- Slow
deleteNAt :: (Ord a) => Int -> Int -> Ring a -> (Ring a, Seq a)
deleteNAt ix n ring = dN n [] ring
  where
    dN 0 removed ring = (ring, SQ.fromList (reverse removed))
    dN n removed ring = let (ring', r) = deleteAt ix ring in dN (n -1) (r : removed) ring'

move :: Int -> Int -> Int -> Ring Int -> Ring Int
move step target cix ring@(Ring s _)
  | step == target = ring
  | otherwise =
    trace (show (step, c, [r1, r2, r3], d)) $
      move (step + 1) target ((ixOf c nextRing + 1) `mod` length s) nextRing
  where
    --trace (show (step, cups, c, [r1, r2, r3], rest, (d, dIx), a, b, snd <$> sort (M.toList ixs))) $
    --trace (show (step, length a, length b, hit, dIx, d)) $
    --(step + 1, knownIxs, nextCups)
    --trace (show step) $
    -- (step + 1, nextIxs, nextCups)

    c = get cix ring
    (ringWithout, rs@(r1 :<| r2 :<| r3 :<| _)) = deleteNAt (cix + 1) 3 ring
    ds' = [c - 1, c - 2, c - 3, c - 4]
    ds = (\d -> if d <= 0 then d + length s else d) <$> ds'
    d = head $ filter (not . (`elem` [r1, r2, r3])) ds
    dIx = ixOf d ringWithout
    nextRing = insertNAt (dIx + 1) rs ringWithout

-- fuzzy find in region
matchWithin :: Int -> Int -> Int -> Seq Int -> Maybe Int
matchWithin threshold d ix cups =
  case SQ.elemIndexL d window of
    Just i -> Just (length pre + i)
    Nothing -> Nothing
  where
    (pre, splitCups) = SQ.splitAt (ix - threshold) cups
    window = SQ.take (2 * threshold) splitCups
    dIx = SQ.elemIndexR d window

move' :: Int -> Int -> M.Map Int (Int, Int) -> Seq Int -> Seq Int
move' step target ixs cups@(c :<| r1 :<| r2 :<| r3 :<| _)
  | step == target = cups
  | otherwise =
    trace (show (step, c, [r1, r2, r3], d, "lastDix", dIx', "nowDix", dIx, "age", age, "lastsaw", step', hit, hit2)) $
      --trace (show (step)) $
      move' (step + 1) target nextIxs nextCups
  where
    ds' = [c - 1, c - 2, c - 3, c - 4]
    ds = (\d -> if d <= 0 then d + length cups else d) <$> ds'
    d = head $ filter (not . (`elem` [r1, r2, r3])) ds
    (dIx', age, step') =
      case M.lookup d ixs of
        Just (ix, step') -> (ix, step - step', step')
        Nothing -> let Just ix = SQ.elemIndexR d cups in (ix, 0, 0)
    -- Nothing -> let Just ix = SQ.elemIndexR d cups in (ix, -1)
    --dIx = if age > 1 then length cups - age else dIx'
    dIxH' = if age > 1 then dIx' + 1 - age else dIx'
    (dIxH, hit) =
      case matchWithin 20 d dIxH' cups of
        Just ix -> (ix, True)
        Nothing -> let Just ix = SQ.elemIndexR d cups in (ix, False)

    (dIx, hit2) = let d' = cups `SQ.index` dIxH in if d' == d then (dIxH, True) else let Just ix = SQ.elemIndexR d cups in (ix, False)
    (_ :<| _ :<| _ :<| _ :<| a, b) = SQ.splitAt (dIx + 1) cups
    nextCups = a >< ((r1 <| r2 <| r3 <| b) |> c)
    nextIxs =
      foldl'
        (\acc (k, v) -> M.insert k v acc)
        ixs
        [ (r1, (length a, step)),
          (r2, (length a + 1, step)),
          (r3, (length a + 2, step)),
          (c, (length cups - 1, step))
        ]

{-
we have 1M pointers to linked list nodes
pointer starts at first
follow next 3 times, cut out, set next cyclicly
look up dest, which is easy
chop in 3x after dest
set pointer to be original's current next
-}

-- all but last
mkNodeMap' :: [Int] -> IO [(Int, IORef Int)]
mkNodeMap' [_] = return []
mkNodeMap' (n1 : n2 : s) = do
  ref <- newIORef n2
  rest <- mkNodeMap' (n2 : s)
  return $ (n1, ref) : rest

mkNodeMap :: [Int] -> IO (M.Map Int (IORef Int))
mkNodeMap (n : ns) = do
  nodes <- M.fromList <$> mkNodeMap' (n : ns)
  cycleRef <- newIORef n
  return $ M.insert (last ns) cycleRef nodes

nextNode :: M.Map Int (IORef Int) -> Int -> IO Int
nextNode nexts i = readIORef (nexts M.! i)

setNextNode :: M.Map Int (IORef Int) -> Int -> Int -> IO ()
setNextNode nexts n1 n2 = writeIORef (nexts M.! n1) n2

moveP :: M.Map Int (IORef Int) -> Int -> IO (M.Map Int (IORef Int), Int)
moveP nexts c = do
  r1 <- nextNode nexts c
  r2 <- nextNode nexts r1
  r3 <- nextNode nexts r2
  r4 <- nextNode nexts r3
  setNextNode nexts c r4
  let ds' = [c - 1, c - 2, c - 3, c - 4]
      ds = (\d -> if d <= 0 then d + M.size nexts else d) <$> ds'
      d = head $ filter (not . (`elem` [r1, r2, r3])) ds
  dNext <- nextNode nexts d
  setNextNode nexts d r1
  setNextNode nexts r3 dNext
  cNext <- nextNode nexts c
  return (nexts, cNext)

readOut :: M.Map Int (IORef Int) -> Int -> IO [Int]
readOut nexts n = do
  n' <- nextNode nexts n
  rest <- readOut nexts n'
  return (n : rest)

movePSteps :: Int -> Int -> M.Map Int (IORef Int) -> Int -> IO (M.Map Int (IORef Int))
movePSteps steps target nexts c
  | steps == target = return nexts
  | otherwise = do
    (nexts', c') <- moveP nexts c
    print steps
    movePSteps (steps + 1) target nexts' c'

part1P :: IO String
part1P = do
  nodes <- mkNodeMap input
  movePSteps 0 100 nodes (head input)
  ans <- readOut nodes 1
  return $ intToDigit <$> drop 1 (take 9 ans)

read2 :: M.Map Int (IORef Int) -> IO (Int, Int)
read2 nexts = do
  n1 <- nextNode nexts 1
  n2 <- nextNode nexts n1
  return (n1, n2)

part2P :: IO Int
part2P = do
  nodes <- mkNodeMap longInput
  nodes' <- movePSteps 0 10000000 nodes (head input)
  (n1, n2) <- read2 nodes'
  return (n1 * n2)

{-
--dIx = 1000
-- dIx = ixs M.! d - step
-- dIx = ixs M.! d
(_ :<| _ :<| _ :<| _ :<| a, b) = SQ.splitAt (dIx + 1) cups
--(a, b) = SQ.splitAt (dIx + 1) rest
nextCups = a >< ((r1 <| r2 <| r3 <| b) |> c)
-- can we keep any of the old ixs?
-- no, gets too slow
knownIxs =
  M.fromList
    [ (r1, length a),
      (r2, length a + 1),
      (r3, length a + 2),
      (c, length cups)
    ]
ixUpdates =
  [(c, const $ length cups)]
    ++ [(cup, const i) | (i, cup) <- zip [0 ..] (F.toList a)]
    ++ [(r, const (length a + i)) | (i, r) <- zip [0 ..] [r1, r2, r3]]
-- ++ [(cup, const (length a + 3 + i)) | (i, cup) <- zip [0 ..] (F.toList b)]
nextIxs = foldl' (\acc (k, f) -> M.adjust f k acc) ixs ixUpdates
-}

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
    . ringToSeq
    $ move 0 100 0 (mkRing input)

part1' :: Int
part1' =
  read
    . drop 1
    . fmap intToDigit
    . F.toList
    . alignTo1
    $ move' 0 100 M.empty (SQ.fromList input)

longInput :: [Int]
longInput = input ++ [10 .. 1000000]

part2 :: Int
part2 =
  product
    . take 2
    . drop 1
    . F.toList
    . alignTo1
    . ringToSeq
    $ move 0 10000000 0 (mkRing longInput)

part2' :: Int
part2' =
  product
    . take 2
    . drop 1
    . F.toList
    . alignTo1
    $ move' 0 10000000 M.empty (SQ.fromList longInput)
