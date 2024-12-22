module Day21 where

type NumPad = ".A0123456789"

type DirPad = ".A^v<>"

numPad :: NumPad ▦ ℤ²
numPad = readGrid "789\n456\n123\n.0A"

dirPad :: DirPad ▦ ℤ²
dirPad = readGrid ".^A\n<v>"

type Paths cs = (Cell cs, Cell cs) :|-> [[Cell DirPad]]

paths :: (SChar '.' :< SymSChars cs, Ord (Cell cs), GridCell (Cell cs)) => cs ▦ ℤ² -> Paths cs
paths g = shortest ∘ sortOn size <$> go (mk [(s, s, []) | s <- coords g])
  where
    shortest ps@(p : _) = takeWhile ((≡ size p) ∘ size) ps
    go Empty = mkMap [((c, d), []) | c <- cells g, d <- cells g]
    go ((s, c, dirs) :<| q)
      | g |! c ≡ (#"." □) = go q
      | size dirs >= ((+) $@ gridDims g) - 1 = go q
      | otherwise =
          let ns = [(s, n, dirs <> (fromChar ∘ toArrow² <$> goingTo c n)) | n <- neighs @4 c g]
           in go (q >< mk ns) |~ ((g |! s, g |! c), (dirs :))

(numPaths, dirPaths) :: (Paths NumPad, Paths DirPad) = (paths numPad, paths dirPad)

presses :: ℤ -> Cell NumPad -> Cell NumPad -> MM (ℤ, [Cell DirPad]) (Σ ℤ)
presses layers = go numPaths 0
  where
    go :: (Ord (Cell cs)) => Paths cs -> ℤ -> Cell cs -> Cell cs -> MM (ℤ, [Cell DirPad]) (Σ ℤ)
    go paths layer from to = minimum <$> forM (paths |! (from, to)) (\path -> go' .$. (layer, (path <> [(#A □)])))
    go' ((≡ layers) -> True, path) = return ∘ Σ $ size path
    go' (layer, path) = sum <$> forM (zip ((#A □) : path) path) (&@ go dirPaths (layer + 1))

complexity :: ℤ -> Text -> Σ ℤ
complexity layers code =
  let cs = unpack code
      bs = fromChar <$> cs
      value = Σ $ take 3 cs |- number
      go start b = run (presses layers start b)
   in value ⋅ fst (foldl' (\(ps, start) b -> (ps + go start b, b)) (Σ 0, (#A □)) bs)

part1 :: Σ ℤ
part1 = (complexity 2 <$> (lines $(aoc 21)) <>!)

part2 :: Σ ℤ
part2 = (complexity 25 <$> (lines $(aoc 21)) <>!)
