module TwentySixteen.Day14 where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.List (nub)

salt :: String
salt = "zpqevtbw"

hashString :: String -> String
hashString = BC.unpack . B16.encode . MD5.hash . BC.pack

hashes :: Int -> [String]
hashes n =
  foldr (.) id (replicate n hashString)
    . (salt ++)
    . show
    <$> [0 ..]

has3 :: Eq a => [a] -> Maybe a
has3 [_, _] = Nothing
has3 (a : b : c : xs) =
  if length (nub [a, b, c]) == 1
    then Just a
    else has3 (b : c : xs)

has5 :: Eq a => a -> [a] -> Bool
has5 _ [_, _, _, _] = False
has5 x (a : b : c : d : e : xs) =
  (a == x && length (nub [a, b, c, d, e]) == 1)
    || has5 x (b : c : d : e : xs)

isKey :: [(Int, String)] -> Bool
isKey ((_, h) : hs) =
  case has3 h of
    Nothing -> False
    Just a -> any (has5 a) (snd <$> take 1000 hs)

getAllKeys :: [(Int, String)] -> [(Int, String)]
getAllKeys all@(h : hs)
  | isKey all = h : getAllKeys hs
  | otherwise = getAllKeys hs

part1 :: Int
part1 = fst $ getAllKeys (zip [0 ..] $ hashes 1) !! 63

part2 :: Int
part2 = fst $ getAllKeys (zip [0 ..] $ hashes 2017) !! 63
