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

presses :: ℤ -> Cell Numpad -> Cell Numpad -> MM (ℤ, [Cell Dirpad]) ℤ
presses layers target start =
  minimum <$> forM (numPaths |! (start, target)) (\path -> go .$. (0, (path <> [(#A □)])))
  where
    go ((≡ layers) -> True, path) = return (size path)
    go (layer, path) =
      sum <$> do
        forM (zip ((#A □) : path) path) $ \(from, to) ->
          minimum <$> (forM (dirPaths |! (from, to)) (\path -> go .$. (layer + 1, path <> [(#A □)])))

complexity :: [Text] -> ℤ -> [Σ ℤ]
complexity codes layers =
  [ Σ ∘ (⋅ value) ∘ fst $
      foldl' (\(ps, start) b -> (ps + run (presses layers b start), b)) (0, (#A □)) bs
    | code <- codes,
      let cs = unpack code,
      let bs = [fromChar c | c <- cs],
      let value = take 3 cs |- number
  ]

part1 :: Σ ℤ
part1 = (complexity (lines $(aoc 21)) 2 <>!)

part2 :: Σ ℤ
part2 = (complexity (lines $(aoc 21)) 25 <>!)
