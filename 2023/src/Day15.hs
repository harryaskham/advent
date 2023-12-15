module Day15 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.Sequence qualified as SQ

hash :: String -> Int
hash =
  let go n [] = n
      go n (c : cs) = go (((n + ord c) * 17) `mod` 256) cs
   in go 0

data Operation = OpSet String Int | OpDel String

getLabel :: Operation -> String
getLabel (OpSet l _) = l
getLabel (OpDel l) = l

op :: Parser Operation
op = do
  l <- many1 alphaNum
  (OpSet l <$> (char '=' *> number)) <|> (OpDel l <$ char '-')

data Box = Box (Map String Int) (Seq String)

boxOp (OpSet l f) (Box m s)
  | l |∈ m = Box (m |. (l, f)) s
  | otherwise = Box (m |. (l, f)) (s |> l)
boxOp (OpDel l) b@(Box m s)
  | l |∈ m = Box (M.delete l m) (SQ.deleteAt (uhead $ SQ.elemIndicesL l s) s)
  | otherwise = b

run :: Map Int Box -> Operation -> Map Int Box
run m o = m |~ (hash (getLabel o), boxOp o)

power :: (Int, Box) -> Int
power (i, Box m s) = sum [(i + 1) * slot * (m |! l) | (slot, l) <- zip [1 ..] (unSeq s)]

part1 :: Int
part1 =
  $(input 15)
    |- (many (noneOf ",") `sepBy` char ',')
    & fmap hash
    & sum

part2 :: Int
part2 =
  $(input 15)
    |- (op `sepBy` char ',')
    & foldl' run (mkMap [(i, Box (mkMap []) (mkSeq [])) | i <- [0 .. 255]])
    & (unMap >>> fmap power >>> sum)
