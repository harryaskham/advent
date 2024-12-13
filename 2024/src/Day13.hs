module Day13 (part1, part2) where

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

part1 :: ℤ
part1 = $(aoc 13) |-..<> numbers @ℤ & minimize

o = 10000000000000

-- how many presses to traverse at least 100...
-- how far do we overshoot
-- subtract this to get new prize
-- hmm this is its own distribution
-- new relationship
-- this is equivalent to part 1 on (o,o)

part2 :: Text
part2 = "Part 2"
