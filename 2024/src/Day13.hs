module Day13 where

-- tok = 3*a + b
-- px = a*ax + b*bx
-- py = a*ay + b*by
-- take smallest b that might solve
-- find a that does
-- i.e. b = (px - a*ax) / bx
--      b = (py - a*ay) / by
--- with b fixed then we have
--- (px - a*ax) * by = (py - a*ay) * bx
--  px ⋅ bx - a ⋅ ax ⋅ by = py ⋅ bx - a ⋅ ay ⋅ bx
-- a ⋅ ay ⋅ bx - a ⋅ ax ⋅ by = py ⋅ bx - px ⋅ bx
-- a (ay ⋅ bx - ax ⋅ bx) = (py - px) ⋅ bx
-- a = (py - px) ⋅ bx / (ay * bx - ax ⋅ bx)
-- but we also have simply that
-- a = (px - b*bx) / ax
-- a = (py - b*by) / ay

minimize :: [ℤ] -> ℤ
minimize [] = 0
minimize (ax : ay : bx : by : px : py : claws) =
  case [ 3 ⋅ a + b
         | a <- [0 .. 100],
           b <- [0 .. 100],
           a ⋅ ax + b ⋅ bx ≡ px ∧ a ⋅ ay + b ⋅ by ≡ py
       ] of
    [] -> 0
    ts -> minimum ts
    + minimize claws

minimize2 :: ℤ -> [ℤ] -> ℤ
minimize2 _ [] = 0
minimize2 o (ax : ay : bx : by : px' : py' : claws) =
  let (px, py) = (px', py') + (o, o)
   in case [ 3 ⋅ (a + o) + (b + o)
             | -- let maxb :: ℤ = max (px `div` bx) (py `div` by),
               let r = 100,
               b <- [-r, -r + 1 .. r],
               -- b <- [maxb, maxb - 1 .. 0],
               let (pxa, pya) = traceShow b $ (px - b ⋅ bx, py - b ⋅ by),
               let (a, amod) = divMod @ℤ pxa ax,
               traceShow (a, amod) $ amod ≡ 0,
               let (a', amod') = divMod @ℤ pya ay,
               traceShow (a', amod') $ amod' ≡ 0,
               a ≡ a'
           ] of
        [] -> 0
        (presses : _) -> presses
        + minimize2 o claws

dist n [ax, ay, bx, by, px, py] =
  traceShow ("a", (ax, ay), "b", (bx, by), "p", (px, py)) $
    [ (pos, 3 ⋅ a + b)
      | let bmid = max (px `div` bx) (py `div` by),
        b <- traceShow ("b mid", bmid) $ [bmid - n, bmid - n + 1 .. bmid + n],
        let pxa = px - b ⋅ bx,
        let pya = py - b ⋅ by,
        let ((a, am), (a', am')) = (pxa `divMod` ax, pya `divMod` ay),
        traceShow ("off by", (px - a ⋅ ax - b ⋅ bx, py - a ⋅ ay - b ⋅ by)) $
          a ≡ a' ∧ am ≡ 0 ∧ am' ≡ 0,
        let pos =
              (a ⋅ ax + b ⋅ bx, a ⋅ ay + b ⋅ by)
    ]

dists n = let s = dist n <$> claws in (s, sum (sum <$> (snd <$$> s)))

-- get close as possible in b steps
-- backtrack for a steps

close :: ℝ² -> ℝ² -> ℝ
close (px, py) (bx, by) = min (px / bx) (py / by)

opt :: [ℤ] -> ℤ
opt claw@[ax', ay', bx', by', px', py'] = go 0.0 (0.0, 0.0) -- close (px, py) (bx, by))
  where
    [ax, ay, bx, by, px, py] = fromIntegral <$> claw
    p' (a, b) = (a ⋅ ax' + b ⋅ bx', a ⋅ ay' + b ⋅ by')
    p (a, b) = (a ⋅ ax + b ⋅ bx, a ⋅ ay + b ⋅ by)
    loss (a, b) = (a ⋅ ax + b ⋅ bx - px) ** 2 + (a ⋅ ay + b ⋅ by - py) ** 2 + 3 * a + b
    grad (a, b) =
      ( 2 ⋅ ax ⋅ (a ⋅ ax + b ⋅ bx - px) + 2 ⋅ ay ⋅ (a ⋅ ay + b ⋅ by - py) + 3,
        2 ⋅ bx ⋅ (a ⋅ ax + b ⋅ bx - px) + 2 ⋅ by ⋅ (a ⋅ ay + b ⋅ by - py) + 1
      )
    go :: ℝ -> ℝ² -> ℤ
    go last (a, b)
      | l ≡ last =
          traceShow ("nothing", (px', py')) $
            0
      | p' (a', b') ≡ (px', py') =
          traceShow ("found", (a', b')) $
            3 ⋅ a' + b'
      | otherwise =
          ( if traceOn
              then
                ( traceShow ("prize", (px', py'))
                    . traceShow ("claw", p' (a', b'))
                    . traceShow ("grad", (da', db'))
                    . traceShow ("gradscaled", (da, db))
                    . traceShow ("a',b'", (a', b'))
                    . traceShow ("a,b", (a, b))
                    . traceShow ("loss", l)
                    . traceShow ""
                )
              else id
          )
            $ go
              l
              (a - (da / ax), b - (db / bx))
      where
        -- \$ go (a + lr ⋅ ((lx / ax) + (ly / ay)), b - lr ⋅ ((lx / bx) + (ly / by)))
        l = loss (a, b)
        (lx, ly) = (px, py) - p (a, b)
        lr = 0.001
        (da', db') = grad (a, b)
        (da, db) = (lr ⋅ da', lr ⋅ db')
        (a', b') = (round a, round b)

-- & qInsert loss (a + lx `div` ax, b)
-- & qInsert loss (a + lx `div` ax, b)
-- & qInsert loss (a, b + lx `div` bx)
-- & qInsert loss (a + (lx `div` 2) `div` ax, b + (lx `div` 2) `div` bx)
-- & qInsert loss (a + ly `div` ay, b)
-- & qInsert loss (a, b + ly `div` by)
-- & qInsert loss (a + (ly `div` 2) `div` ay, b + (ly `div` 2) `div` by)

-- every a has a set of poss bs and vice versa
-- where
-- (px - a ⋅ ax) ≡ b ⋅ bx
-- (py - a ⋅ ay) ≡ b ⋅ by
-- or
-- (px - b ⋅ bx) ≡ a ⋅ ax
-- (py - b ⋅ by) ≡ a ⋅ ay
-- for a given b:
-- a ≡ (px - b ⋅ bx) / ax ≡ (py - b ⋅ by) / ay
-- fine but we need a to be integral
-- 0 ≡ (px - b ⋅ bx) mod ax ≡ (py - b ⋅ by) mod ay
-- ∃n∈ℤ. n ⋅ ax ≡ px - b ⋅ bx
--       n ≡ (px - b ⋅ bx) / ax
--   or  b ≡ (px - n ⋅ ax) / bx
-- so now we need (px - b ⋅ bx) / ax to be integral
-- or
-- (px - a ⋅ ax) / bx ≡ b (1)
-- (px - b ⋅ bx) / ax ≡ a (2)
-- sub b into (2)
-- (px - ((px - a ⋅ ax) / bx) ⋅ bx) ≡ a
-- (px - ())
-- for b, a = (px `mod` a ⋅ ax ≡ 0, py `mod` a ⋅ ay ≡ 0)
{-
px `mod` b ⋅ bx ≡ 0 → ∃n. n ⋅ b ⋅ bx = px
py `mod` b ⋅ by ≡ 0 → ∃m. m ⋅ b ⋅ by = py
px `mod` a ⋅ ax ≡ 0 → ∃n'. n' ⋅ a ⋅ ax = px
py `mod` a ⋅ ay ≡ 0 → ∃m'. m' ⋅ a ⋅ ay = py
-}

o :: ℤ
o = 10000000000000

claws :: [[ℤ]]
claws = chunksOf 6 $ $(aoc 13) |-..<> numbers @ℤ

bigclaws :: [[ℤ]]
bigclaws = withO <$> claws

withO [ax, ay, bx, by, px, py] = [ax, ay, bx, by, o + px, o + py]

xclaws :: [[ℤ]]
xclaws = chunksOf 6 $ $(aocx 13) |-..<> numbers @ℤ

sanity :: IO ()
sanity = do
  let solves = [opt claw | claw <- withO <$> xclaws]
  mapM_ print solves
  print $ sum solves

cl :: [ℤ]
cl = claws !! 0

clx0 = xclaws !! 0

clx1 = xclaws !! 1

part1 :: ℤ
part1 = 123

-- \$(aoc 13) |-..<> numbers @ℤ & minimize2 0

-- how many presses to traverse at least 100...
-- how far do we overshoot
-- subtract this to get new prize
-- hmm this is its own distribution
-- new relationship
-- this is equivalent to part 1 on (o,o)

-- 97580935862542 too low
-- so either we miss an entire machine
-- or 1+ machines got there but with too few tokens

part2 :: ℤ
part2 = 123

-- \$(aoc 13) |-..<> numbers @ℤ & minimize2 10000000000000

traceOn = False
