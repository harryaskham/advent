# Advent of Code

![](300.png)

Complete solutions to Advent of Code problems in Haskell.

- All years are fully completed. I aimed for idiomatic and relatively clean solutions.
- 2019 was the first year I attemped - it's fully solved, with days 1-18 being less clean, and 19-25 being those that I came back to revisit a year later.

The modules `Coord` and `Grid` contain various tools for working with 2D and 3D grids, and `Utils` contains a grab-bag of generally AOC-friendly tooling.

2015 through 2020 are contained in one huge Stack project; this has become impossible to upgrade, and is pegged to GHC 8.6.5, which doesn't work well with new versions of OS X.

Going forwards from 2021, each year is its own Nix-managed Cabal project.
