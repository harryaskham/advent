module TwentySeventeen.Day16 where

import Control.Arrow ((>>>))
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    choice,
    digit,
    eof,
    letter,
    many1,
    sepBy,
    try,
  )
import Util (readWithParser)

inputPath :: String
inputPath = "input/2017/16.txt"

data Programs = Programs (M.Map Int Char) (M.Map Char Int) Int

mkPrograms :: String -> Programs
mkPrograms cs =
  Programs
    (M.fromList $ zip [0 ..] cs)
    (M.fromList $ zip cs [0 ..])
    0

parseOps :: GenParser Char () (Programs -> Programs)
parseOps = do
  ops <- op `sepBy` char ','
  char '\n'
  eof
  return $ foldl1 (>>>) ops
  where
    op =
      choice $
        try
          <$> [ do
                  char 'x'
                  posA <- many1 digit
                  char '/'
                  posB <- many1 digit
                  return $ exchange (read posA) (read posB),
                do
                  char 's'
                  x <- many1 digit
                  return $ spin (read x),
                do
                  char 'p'
                  a <- letter
                  char '/'
                  b <- letter
                  return $ partner a b
              ]

spin :: Int -> Programs -> Programs
spin x (Programs iToC cToI headI) =
  Programs iToC cToI ((headI - x) `mod` M.size iToC)

exchange :: Int -> Int -> Programs -> Programs
exchange posA posB (Programs iToC cToI headI) =
  Programs iToC' cToI' headI
  where
    posA' = (headI + posA) `mod` M.size iToC
    posB' = (headI + posB) `mod` M.size iToC
    a = iToC M.! posA'
    b = iToC M.! posB'
    iToC' = M.insert posA' b . M.insert posB' a $ iToC
    aC = cToI M.! a
    bC = cToI M.! b
    cToI' = M.insert a bC . M.insert b aC $ cToI

partner :: Char -> Char -> Programs -> Programs
partner a b (Programs iToC cToI headI) =
  Programs iToC' cToI' headI
  where
    posA = cToI M.! a
    posB = cToI M.! b
    cToI' = M.insert a posB . M.insert b posA $ cToI
    aI = iToC M.! posA
    bI = iToC M.! posB
    iToC' = M.insert posA bI . M.insert posB aI $ iToC

toString :: Programs -> String
toString (Programs iToC _ headI) =
  (iToC M.!) . (`mod` M.size iToC)
    <$> [headI .. headI + M.size iToC - 1]

part1 :: IO String
part1 = do
  dance <- readWithParser parseOps <$> readFile inputPath
  return . toString . dance $ mkPrograms ['a' .. 'p']

findCycle :: (Programs -> Programs) -> Int -> Programs -> M.Map (Int, M.Map Int Char) Int -> Int
findCycle dance step p@(Programs iToC _ headI) lastSeen
  | (headI, iToC) `M.member` lastSeen = step
  | otherwise =
    findCycle
      dance
      (step + 1)
      (dance p)
      (M.insert (headI, iToC) step lastSeen)

part2 :: IO String
part2 = do
  dance <- readWithParser parseOps <$> readFile inputPath
  let programs = mkPrograms ['a' .. 'p']
      cycleLength = findCycle dance 0 programs M.empty
  return . toString $
    iterate dance programs !! (1000000000 `rem` cycleLength)
