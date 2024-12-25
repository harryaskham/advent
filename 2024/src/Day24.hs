module Day24 where

import Data.Bimap qualified as BM
import Data.Text qualified as T
import Text.Show qualified as TS

-- z27: one of rvf or dkb is wrong
-- rvf is AND x27 y27, should be XOR x27 y27 → tpc

swaptext =
  T.unpack
    ∘ T.replace "vmr XOR bnc -> hmk" "vmr XOR bnc -> z16"
    ∘ T.replace "y16 AND x16 -> z16" "y16 AND x16 -> hmk"
    ∘ T.replace "pns AND tsc -> z20" "pns AND tsc -> fhp"
    ∘ T.replace "tsc XOR pns -> fhp" "tsc XOR pns -> z20"
    ∘ T.replace "x27 AND y27 -> rvf" "x27 AND y27 -> tpc"
    ∘ T.replace "y27 XOR x27 -> tpc" "y27 XOR x27 -> rvf"
    ∘ T.replace "wkw OR jgr -> z33" "wkw OR jgr -> fcd"
    ∘ T.replace "smf XOR rfd -> fcd" "smf XOR rfd -> z33"
    ∘ T.pack

(initial', circuit') :: (Map String 𝔹, Map String (String, String, String)) =
  -- \$(aocxn 24 2)
  (swaptext $(aoc 24))
    |- ( (,)
           <$> ((mapSep (string ": ") eol abc123 (as @𝔹 <$> number @ℤ)) <* eol)
           <*> ((\m -> mkWith (<>) [(w, op) | (op, ws) <- unMap m, w <- ws]) <$> (mapcat "-> " (tuple @3 (abcABC123 <* spaces)) abc123))
       )

value :: Map String 𝔹 -> Map String String -> Map String (String, String, String) -> String -> 𝔹
value initial swap' circuit w'
  | w ∈ initial = initial |! w
  | otherwise =
      let v = value initial swap' circuit
       in case circuit |! w of
            (w, "AND", y) -> v w ∧ v y
            (w, "OR", y) -> v w ∨ v y
            (w, "XOR", y) -> v w `xor` v y
  where
    w = case swap' |? w' of
      Nothing -> w'
      Just w -> w

to :: ℤ -> Char -> ℤ -> Map String 𝔹
to n c x =
  mkMap
    [ (c : pad 2 '0' (show i), b)
      | (i, b) <- zip [n - 1, n - 2 ..] (pad (as @ℤ₆₄ n) False (intToBits (as @ℤ₆₄ x)))
    ]

x !+! y = go (to n 'x' x <> to n 'y' y) (mkMap []) circuit'

test :: ℤ -> Map String String -> [𝔹]
test n swap' =
  let x !+! y = go (to n 'x' x <> to n 'y' y) swap' circuit'
   in [a !+! b ≡ a + b | a <- [1 .. 10], b <- [1 .. 10]]

tests swps = test 45 (mkMap [(a, b) | [a, b] <- swps])

swaps :: Map String (String, String, String) -> [Map String String]
swaps circuit =
  [ mkMap $ [a, b, c, d] <> (swap <$> [a, b, c, d])
    | let ps = triPairs (keys (circuit)),
      (a@(a0, a1), b@(b0, b1), c@(c0, c1), d@(d0, d1)) <- (,,,) <$> ps <*> ps <*> ps <*> ps,
      nub [a0, a1, b0, b1, c0, c1, d0, d1] ≡ [a0, a1, b0, b1, c0, c1, d0, d1]
  ]

fours :: (Eq a) => [a] -> [[a]]
fours xs =
  [ [a, b, c, d]
    | (a, b, c, d) <- (,,,) <$> xs <*> xs <*> xs <*> xs,
      nub [a, b, c, d] ≡ [a, b, c, d]
  ]

swaps2 :: Map String (String, String, String) -> [Map String String]
swaps2 circuit =
  [ mkMap $ [a, b] <> (swap <$> [a, b])
    | let ps = triPairs (keys (circuit)),
      (a@(a0, a1), b@(b0, b1)) <- (,) <$> ps <*> ps,
      nub [a0, a1, b0, b1] ≡ [a0, a1, b0, b1]
  ]

go :: Map String 𝔹 -> Map String String -> Map String (String, String, String) -> ℤ
go initial swap' circuit =
  bitsToInt $
    snd
      <$> sortOn
        (Down ∘ fst)
        [ (n |- number @ℤ, value initial swap' circuit w)
          | w@(z : n) <- keys circuit,
            z ≡ 'z'
        ]

part1 :: ℤ
part1 = go initial' ø circuit'

printBit :: Map String 𝔹 -> String -> String
printBit initial z
  | z ∈ initial = z
  | otherwise =
      let (w, op, y) = circuit' |! z
       in "(" <> z <> ": " <> (printBit initial w) <> " " <> op <> " (" <> (printBit initial y) <> ")"

indent :: Int -> String -> String
indent n s = unpack (unlines ((pack (replicate n ' ') <>) <$> lines (pack s)))

-- nC2 ⋅ (n-2)C2 ⋅ (n-4)C2 ⋅ (n-6)C2
part2' :: String
part2' =
  let n = 1 + maximum [n |- number @ℤ | (z : n) <- keys circuit', z ≡ 'z']
   in intercalate "," ∘ sort ∘ mconcat $
        [ [a, b]
          | -- \| let swaps' = swaps circuit',
            let swaps' = swaps2 circuit',
            (i, swap') <- zip [0 ..] swaps',
            traceShow (i, size swaps') $ and (test n swap'),
            (a, b) <- unMap swap'
        ]

data Circuit = OR Circuit Circuit | AND Circuit Circuit | XOR Circuit Circuit | N String deriving (Eq, Ord)

-- instance TS.Show Circuit where
--  show (OR a b) = "OR\n" <> indent 1 (show a <> "\n" <> show b)
--  show (XOR a b) = "XOR\n" <> indent 1 (show a <> "\n" <> show b)
--  show (AND a b) = "AND\n" <> indent 1 (show a <> "\n" <> show b)
--  show (N a) = a
instance TS.Show Circuit where
  show (OR a b) = "(OR " <> show a <> " " <> show b <> ")"
  show (XOR a b) = "(XOR " <> show a <> " " <> show b <> ")"
  show (AND a b) = "(AND " <> show a <> " " <> show b <> ")"
  show (N a) = a

getTree :: String -> Circuit
getTree z =
  case circuit' |? z of
    Just (w, "AND", y) -> AND (getTree w) (getTree y)
    Just (w, "OR", y) -> OR (getTree w) (getTree y)
    Just (w, "XOR", y) -> XOR (getTree w) (getTree y)
    Nothing -> N z

oneDs [] [] = []
oneDs a [] = []
oneDs [] b = []
oneDs a b = a

diffTree :: Circuit -> Circuit -> [(Circuit, Circuit)]
diffTree a b = go a b
  where
    go :: Circuit -> Circuit -> [(Circuit, Circuit)]
    go (AND a b) (AND a' b') = oneDs (go a a') (go a b') <> oneDs (go b b') (go b a')
    go (OR a b) (OR a' b') = oneDs (go a a') (go a b') <> oneDs (go b b') (go b a')
    go (XOR a b) (XOR a' b') = oneDs (go a a') (go a b') <> oneDs (go b b') (go b a')
    go (N a) (N b)
      | a ≡ b = []
      | otherwise = [(N a, N b)]
    go a b = [(a, b)]

treeIn t t'@(AND a b) = (diffTree t t' ≡ []) ∨ (treeIn t a) ∨ (treeIn t b)
treeIn t t'@(OR a b) = (diffTree t t' ≡ []) ∨ (treeIn t a) ∨ (treeIn t b)
treeIn t t'@(XOR a b) = (diffTree t t' ≡ []) ∨ (treeIn t a) ∨ (treeIn t b)
treeIn t t'@(XOR a b) = (diffTree t t' ≡ []) ∨ (treeIn t a) ∨ (treeIn t b)
treeIn t t' = diffTree t t' ≡ []

m = (mkBimap [(z, getTree z) | z <- keys circuit'])

oneswap :: String -> String -> BM.Bimap String Circuit -> BM.Bimap String Circuit
oneswap a b =
  BM.insert a (m BM.! b) . BM.insert b (m BM.! a)

n = 1 + maximum [n |- number @ℤ | (z : n) <- keys circuit', z ≡ 'z']

dm' = [diffTree (m BM.! (z na)) (m BM.! (z nb)) | (na, nb) <- pairs [0 .. n - 1]]

trees = [z <> " =\n" <> (show g) | (z, g) <- unBimap m]

x i = N ('x' : pad 2 '0' (show i))

y i = N ('y' : pad 2 '0' (show i))

halfadd a b = (XOR a b, AND a b)

add1 a b c =
  let (p, c0) = halfadd a b
      (s, c1) = halfadd p c
   in (s, OR c0 c1)

cin 0 = N "()"
cin 1 = AND (x 0) (y 0)
cin i = snd $ add1 (x (i - 1)) (y (i - 1)) (cin (i - 1))

radd 0 = XOR (x 0) (y 0)
radd i = fst $ add1 (x i) (y i) (cin i)

z i = ('z' : pad 2 '0' (show i))

diffit i = (diffTree (m BM.! (z i)) (radd i))

real i = m BM.! (z i)

getk k = m BM.! k

gett k = getclosest (getk k)

exS k = getclosest (radd k)

exC k = getclosest (cin k)

getclosest :: Circuit -> [(String, Circuit)]
getclosest cir = [(name, cir') | (name, cir') <- unBimap m, diffTree cir cir' ≡ []]

divs = [(fst <$> getclosest da, fst <$> getclosest db) | i <- [0 .. 44], let d = diffit i, not (null d), (da, db) <- d]

prs = (catMaybes . (\(a, b) -> [a !? 0, b !? 0]) <$> divs) & filter ((== 2) . size) & fmap sort & sort & nub

ordr a b
  | diffTree a b ≡ [] = EQ
  | a `treeIn` b = LT
  | otherwise = GT

ordrT a b = ordr (getk a) (getk b)

fourpairs = prs & sortBy (\a b -> ordrT (a !! 0) (b !! 0)) & fours

prtests = [(quad, ts) | quad <- fourpairs, let ts = tests quad, or ts]

part2 :: String
part2 = intercalate "," $ sort ["hmk", "z16", "z20", "fhp", "rvf", "tpc", "z33", "fcd"]

-- part2 :: IO ()
-- part2 =
--   do
--     -- mapM_ putStrLn ["Real:\n" <> show (m BM.! (z i)) <> "\nConstructed:\n" <> show (radd i) <> "\n\n\n" | i <- [0 .. n - 1]]
--     mapM_ putStrLn [show (i, abs, size abs) | i <- [0 .. n - 1], let abs = diffTree (m BM.! (z i)) (radd i)]

-- wdk: (AND OR AND x40 y40) should be (AND XOR )
{-
z38 first gate looks at 35: XOR OR AND OR AND OR AND 35
z25 frst gate looks at 23: XOR OR AND OR AND XOR 23
z23 first gae looks at 21: XOR OR AND OR AND 21
z10 at 8: XOR OR AND OR AND 8
z07 at 4:

y16 AND x16 → z16
hmk exactly matches z16

wkw OR jgr -> z33
jgr is a carry-out bit

pns AND tsc → z20

-}
