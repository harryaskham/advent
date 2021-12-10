module Day10 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Helper.TH (input)
import Helper.Util (median)

ltr :: Map Char Char
ltr = M.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

score :: Char -> Maybe (Int, Int)
score ')' = Just (3, 1)
score ']' = Just (57, 2)
score '}' = Just (1197, 3)
score '>' = Just (25137, 4)
score _ = Nothing

parseLine :: String -> Either Char String
parseLine cs' = go cs' []
  where
    go [] s = Right s
    go (c : cs) s
      | c `M.member` ltr = go cs (ltr M.! c : s)
      | otherwise = case s of
        [] -> Left c
        (c' : s') -> if c == c' then go cs s' else Left c

parsedInput :: ([Char], [String])
parsedInput = partitionEithers (parseLine . T.unpack <$> lines $(input 10))

scoreLine :: String -> Maybe Int
scoreLine = foldl' (\s c -> (+) <$> (snd <$> score c) <*> ((* 5) <$> s)) (Just 0)

part1 :: Int
part1 = sum (fst <$> mapMaybe score (fst parsedInput))

part2 :: Int
part2 = median (mapMaybe scoreLine (snd parsedInput))
