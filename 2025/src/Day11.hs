module Day11 (part1, part2) where

g :: 𝕊 :|-> [𝕊] = $(aoc 11) & (⋮) @(MapSep ": " (𝕊 ⭀ AaZz) [𝕊 ⭀ AaZz])

part1 :: ℤ = let go n = n == "out" ??? 1 $ sum (go <$> g |! n) in go "you"

part2 :: ℤ = 
    let go (dac, fft, "out") = pure (dac && fft ??? 1 $ 0)
        go (dac, fft, n) = sum <$> sequence [go .$. (dac || (n == "dac"), fft || (n == "fft"), n') | n' <- g |! n]
    in run $ go (False, False, "svr")
