module Day8 (part1, part2) where

import Data.List (foldl1)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Helper.TH (input)
import Helper.Util (eol, parseWith)
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec (Parser, alphaNum, count, eof, many1, oneOf, string)

parser :: Parser ([Char], Map String (String, String))
parser =
  let name = count 3 alphaNum
   in (,)
        <$> (many1 (oneOf "LR") <* eol <* eol)
        <*> ( M.fromList
                <$> many1
                  ( (,)
                      <$> (name <* string " = (")
                      <*> ( (,)
                              <$> (name <* string ", ")
                              <*> (name <* string ")" <* eol)
                          )
                  )
                <* eof
            )

travel :: (String -> Bool) -> String -> String -> Map String (String, String) -> [(Int, String)]
travel p s lr' g = go (cycle lr') 0 s
  where
    go (d : lr) n c
      | p c = (n, c) : next
      | otherwise = next
      where
        next = case d of
          'L' -> go lr (n + 1) (fst $ g M.! c)
          'R' -> go lr (n + 1) (snd $ g M.! c)

part1 :: Int
part1 =
  $(input 8)
    & parseWith parser
    & uncurry (travel (== "ZZZ") "AAA")
    & U.head
    & fst

part2 :: Int
part2 =
  $(input 8)
    & parseWith parser
    & ( \(lr, g) ->
          fst . U.head
            <$> [ travel ((U.!! 2) >>> (== 'Z')) s lr g
                  | s <- [s | (s, _) <- M.toList g, s U.!! 2 == 'A']
                ]
      )
    & foldl1 lcm
