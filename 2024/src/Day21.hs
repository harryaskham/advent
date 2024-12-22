module Day21 where

type Numpad = ".A0123456789"

type Dirpad = ".A^v<>"

numpad :: Numpad ▦ ℤ²
numpad = readGrid "789\n456\n123\n.0A"

dirpad :: Dirpad ▦ ℤ²
dirpad = readGrid ".^A\n<v>"

type Paths cs = (Cell cs, Cell cs) :|-> [[Cell Dirpad]]

paths :: (SChar '.' :< SymSChars cs, Ord (Cell cs), GridCell (Cell cs)) => cs ▦ ℤ² -> Paths cs
paths g = shortest ∘ sortOn size <$> go (mk [(s, s, []) | s <- coords g])
  where
    shortest ps@(p : _) = takeWhile ((≡ size p) ∘ size) ps
    go Empty = mkMap [((c, d), []) | c <- cells g, d <- cells g]
    go ((s, c, dirs) :<| q)
      | g |! c ≡ (#"." □) = go q
      | size dirs >= ((+) $@ gridDims g) - 1 = go q
      | otherwise =
          let ns = [(s, n, dirs <> (fromChar ∘ toArrow² <$> (goingTo c n))) | n <- neighs @4 c g]
           in go (q >< mk ns) |~ ((g |! s, g |! c), (dirs :))

(numPaths, dirPaths) :: (Paths Numpad, Paths Dirpad) = (paths numpad, paths dirpad)

presses :: ℤ -> Cell Numpad -> Cell Numpad -> MM (ℤ, [Cell Dirpad]) (Σ ℤ)
presses layers = f numPaths 0
  where
    f :: (Ord (Cell cs)) => Paths cs -> ℤ -> Cell cs -> Cell cs -> MM (ℤ, [Cell Dirpad]) (Σ ℤ)
    f paths layer from to = minimum <$> forM (paths |! (from, to)) (\path -> go .$. (layer, (path <> [(#A □)])))
    go ((≡ layers) -> True, path) = return ∘ Σ $ size path
    go (layer, path) = sum <$> forM (zip ((#A □) : path) path) (&@ f dirPaths (layer + 1))

complexity :: [Text] -> ℤ -> [Σ ℤ]
complexity codes layers =
  [ value ⋅ fst (foldl' (\(ps, start) b -> (ps + pressB start b, b)) (Σ 0, (#A □)) bs)
    | code <- codes,
      let cs = unpack code,
      let bs = fromChar <$> cs,
      let value = Σ $ take 3 cs |- number,
      let pressB start b = run (presses layers start b)
  ]

part1 :: Σ ℤ
part1 = (complexity (lines $(aoc 21)) 2 <>!)

part2 :: Σ ℤ
part2 = (complexity (lines $(aoc 21)) 25 <>!)
