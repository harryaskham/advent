module Day20 (part1, part2) where

-- cheats :: ".#SE" ▦ ℤ² -> (Maybe ℤ², Maybe ℤ²) :|-> ([ℤ²], ℤ)
-- cheats g = go h cap (mkQ₁ h (start, [start], ø, True, Nothing, Nothing)) (ø :: Set (ℤ², 𝔹)) ø
cheats :: ".#SE" ▦ ℤ² -> ℤ -> ℤ -> ℤ
cheats g duration threshold = size uniqueCheats
  where
    (start, end) = both (g |!>) (mkC @'S' @".#SE", mkC @'E' @".#SE")
    ((basepath, _) : _) = values $ go (const 0) ꝏ (mkQ₁ (const 0) (start, [start], ø, False, Just start, Just start)) ø (ø :: (Maybe ℤ², Maybe ℤ²) :|-> ([ℤ²], ℤ))
    cap = size basepath
    c2n = mkMap (zip basepath [0 ..])
    nsCheat' to i c = go (mk₁ (c, 0)) ø
      where
        go Empty _ = ø
        go ((c, i) :<| q) seen
          | i ≡ to = ø
          | c ∈ seen = go q seen
          | otherwise =
              let ends = mk [(n, i + 1) | n <- neighs @4 c g, g |! n ≢ (mkC @'#' @".#SE"), n ∈ c2n]
                  next = mk [(n, i + 1) | n <- neighs @4 c g]
               in ends >< go (q >< next) (c |-> seen)
    nsCheat c = unMap $ mkWith min $ un $ nsCheat' duration 0 c
    cheatTimes c = [cost + (c2n |! n) | (n, cost) <- nsCheat c]
    timeSaved c = [(c2n |! c) - ct | ct <- cheatTimes c]
    allSavings = [t | c <- basepath, t <- timeSaved c]
    uniqueCheats = [t | t <- allSavings, t ≥ threshold]
    h (c, p, _, _, _, _) = let d = size @[ℤ²] p + manhattan c end in if d > cap then ꝏ else d
    go _ _ NullQ _ res = res
    go h cap (st@(c, p, seen, canCheat, chStart, chEnd) :<!! q) gseen res
      | c ≡ end = go h cap q gseen (res |. ((chStart, chEnd), (p, cap - size p)))
      | c ∈ seen = go h cap q gseen res
      | (c, canCheat) ∈ gseen = go h cap q gseen res
      | (chStart, chEnd) ∈ res = go h cap q gseen res
      | size p ≥ cap = go h cap q gseen res
      | otherwise =
          -- traceShow (cap, size q, size res) $
          let noCheat = [(n, n : p, c |-> seen, canCheat, chStart, chEnd) | n <- neighs @4 c g, g |! n ≢ (mkC @'#' @".#SE")]
              startCheat = [(n, n : p, c |-> seen, False, Just n, Nothing) | n <- neighs @4 c g, g |! n ≡ (mkC @'#' @".#SE")]
              endCheat = [(n, n : p, c |-> seen, False, chStart, Just n) | n <- neighs @4 c g, g |! n ≢ (mkC @'#' @".#SE")]
              q' =
                qAppend
                  h
                  ( case (chStart, chEnd) of
                      (_, Just _) -> noCheat
                      (Nothing, _) -> noCheat <> startCheat
                      (Just _, _) -> endCheat
                  )
                  q
           in go h cap q' ((c, canCheat) |-> gseen) res

part1 =
  let g = readGrid $(aoc 20)
   in -- (g, threshold) = (readGrid $(aocx 20), 1)
      -- ([Σ 1 | (cheat, (time, saved)) <- unMap (cheats g), saved >= threshold] <>!)
      cheats g 2 100

part2 =
  let g = readGrid $(aoc 20)
   in -- (g, threshold) = (readGrid $(aocx 20), 1)
      -- ([Σ 1 | (cheat, (time, saved)) <- unMap (cheats g), saved >= threshold] <>!)
      cheats g 20 50

-- $> ("expected total save", sum [32,31,29,39,25,23,20,19,12,14,12,22,4,3])
