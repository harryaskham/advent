module Day24 where

import Control.Applicative qualified as CA
import Data.Bimap qualified as BM
import Data.Text qualified as T
import Text.Show qualified as TS

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

inp :: (String -> String) -> (Map String 𝔹, Map String (String, String, String))
inp swaps =
  $(aoc 24)
    |- ( (,)
           <$> ((mapSep (string ": ") eol abc123 (as @𝔹 <$> number @ℤ)) <* eol)
           <*> ((\m -> mkWith (<>) [(swaps w, op) | (op, ws) <- unMap m, w <- ws]) <$> (mapcat "-> " (tuple @3 (abcABC123 <* spaces)) abc123))
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
part1 =
  let (xy, circuit) = inp id
   in go xy ø circuit

indent :: Int -> String -> String
indent n s = unpack (unlines ((pack (replicate n ' ') <>) <$> lines (pack s)))

data Circuit = OR Circuit Circuit | AND Circuit Circuit | XOR Circuit Circuit | N String deriving (Eq, Ord)

part2 :: String
part2 = intercalate "," (sort (keys (fixAdd mempty ? mempty)))
  where
    fixAdd :: String :|-> String -> Maybe (String :|-> String)
    fixAdd swaps
      | size swaps ≡ 8 = Just swaps
      | size swaps > 8 = Nothing
      | otherwise =
          let f z = swaps |? z ? z
              circuit = snd (inp f)
              x !+! y = go (to n 'x' x <> to n 'y' y) (mkMap []) circuit
              m' :: BM.Bimap String (Maybe Circuit)
              m' = mkBimap [(z, getTree circuit z) | z <- keys circuit]
              m :: BM.Bimap String Circuit
              m = (? (N "()")) `BM.mapR` m'
              n = 1 + maximum [n |- number @ℤ | (z : n) <- keys circuit, z ≡ 'z']
              x i = N ('x' : pad 2 '0' (show i))
              y i = N ('y' : pad 2 '0' (show i))
              z i = ('z' : pad 2 '0' (show i))
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
              diffit i = (diffTree (m BM.! (z i)) (radd i))
              diffc i name = (diffTree (m BM.! name) (cin i))
              real i = m BM.! (z i)
              getk k = m BM.! k
              gett k = getclosest (getk k)
              getc k = m BM.!> snd (getclosest (getk k) !! 0)
              exS k = getclosest (radd k)
              exC k = getclosest (cin k)
              getclosest cir =
                [ (name, cir')
                  | (name, cir') <- unBimap m,
                    diffTree cir cir' ≡ []
                ]
              nextS =
                [ (i, s, c, circuit |! (z i), ci)
                  | i <- [0 .. n - 1],
                    let c = exC i,
                    let ci = getc $ (fst (c !! 0)),
                    let s = exS i,
                    null s ∨ (fst (s !! 0)) /= z i
                ]
           in traceShow (swaps) $ case nextS of
                [] -> Just swaps
                ((i, ((sname, _) : _), _, _, _) : _) ->
                  traceShow (sname, z i) $
                    fixAdd (swaps |. (z i, sname) |. (sname, z i))
                ((i, [], ((cname, c) : _), (c0, _, c1), ci) : _) ->
                  let ((((c0, _) : _), _) : (((c1, _) : _), _) : _) = both getclosest <$> diffit i
                   in fixAdd (swaps |. (c0, c1) |. (c1, c0))
                e -> traceShow ("no match", e) $ Nothing

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

getTree :: String :|-> (String, String, String) -> String -> Maybe Circuit
getTree cr = go mempty
  where
    go seen z
      | z ∈ seen = Nothing
      | otherwise =
          let seen' = z |-> seen
           in case cr |? z of
                Just (w, "AND", y) -> AND <$> go seen' w <*> go seen' y
                Just (w, "OR", y) -> OR <$> go seen' w <*> go seen' y
                Just (w, "XOR", y) -> XOR <$> go seen' w <*> go seen' y
                Nothing -> Just $ N z
                _ -> Nothing

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

-- divs = [(fst <$> getclosest da, fst <$> getclosest db) | i <- [0 .. 44], let d = diffit i, not (null d), (da, db) <- d]

-- prs = (catMaybes . (\(a, b) -> [a !? 0, b !? 0]) <$> divs) & filter ((== 2) . size) & fmap sort & sort & nub
