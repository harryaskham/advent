module Day24 where

(initial', circuit') :: (Map String 𝔹, Map String (String, String, String)) =
  -- \$(aocxn 24 2)
  $(aoc 24)
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
      | (i, b) <- zip [n - 1, n - 2 ..] (pad (as @ℤ₆₄ n) False (intToBits x))
    ]

test :: ℤ -> Map String String -> [𝔹]
test n swap' =
  let x !+! y = go (to n 'x' x <> to n 'y' y) swap' circuit'
   in [a !+! b ≡ a + b | a <- [1 .. 10], b <- [1 .. 10]]

swaps :: Map String (String, String, String) -> [Map String String]
swaps circuit =
  [ mkMap $ [a, b, c, d] <> (swap <$> [a, b, c, d])
    | let ps = triPairs (keys (circuit)),
      (a@(a0, a1), b@(b0, b1), c@(c0, c1), d@(d0, d1)) <- (,,,) <$> ps <*> ps <*> ps <*> ps,
      nub [a0, a1, b0, b1, c0, c1, d0, d1] ≡ [a0, a1, b0, b1, c0, c1, d0, d1]
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

printBitPolish :: Map String 𝔹 -> String -> String
printBitPolish initial z
  | z ∈ initial = z
  | otherwise =
      let (w, op, y) = circuit' |! z
       in -- in z <> ":\n" <> op <> "\n" <> indent 1 ("(" <> (printBitPolish initial w) <> ")\n(" <> (printBitPolish initial y) <> ")")
          op <> "\n" <> indent 1 ("(" <> (printBitPolish initial w) <> ")\n(" <> (printBitPolish initial y) <> ")")

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

data Circuit = OR Circuit Circuit | AND Circuit Circuit | XOR Circuit Circuit | N String deriving (Show, Eq, Ord)

getTree :: String -> Circuit
getTree z =
  case circuit' |? z of
    Just (w, "AND", y) -> AND (getTree w) (getTree y)
    Just (w, "OR", y) -> OR (getTree w) (getTree y)
    Just (w, "XOR", y) -> XOR (getTree w) (getTree y)
    Nothing -> N z

part2 :: IO ()
part2 =
  let n = maximum [n |- number @ℤ | (z : n) <- keys circuit', z ≡ 'z']
      x !+! y = to (n + 1) 'x' x <> to (n + 1) 'y' y
   in mapM_
        putStrLn
        [ z <> " =\n" <> (show $ getTree z)
          | i <- [0 .. n - 1],
            let z = ('z' : pad 2 '0' (show i))
        ]

-- wdk: (AND OR AND x40 y40) should be (AND XOR )
{-
z38 first gate looks at 35: XOR OR AND OR AND OR AND 35
z25 frst gate looks at 23: XOR OR AND OR AND XOR 23
z23 first gae looks at 21: XOR OR AND OR AND 21
z10 at 8: XOR OR AND OR AND 8
z07 at 4:
-}
