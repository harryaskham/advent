module TwentySeventeen.Day24 where

import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import qualified Data.Set as S
import Util (toTuple2)

inputPath :: String
inputPath = "input/2017/24.txt"

type Component = (Int, Int)

type Bridge = [Component]

components :: IO [Component]
components =
  fmap (toTuple2 . (fmap read . splitOn "/")) . lines
    <$> readFile inputPath

getBridges :: S.Set Component -> Int -> Bridge -> S.Set Bridge -> S.Set Bridge
getBridges cs leftPort bridge bridges
  | S.null validCs = bridges
  | otherwise = S.foldl' S.union S.empty (next `S.map` validCs)
  where
    next c =
      getBridges
        (S.delete c cs)
        (if fst c == leftPort then snd c else fst c)
        (c : bridge)
        (S.insert (c : bridge) bridges)
    valid (c1, c2) = c1 == leftPort || c2 == leftPort
    validCs = S.filter valid cs

strength :: Bridge -> Int
strength bridge = sum $ ((+) <$> fst <*> snd) <$> bridge

part12 :: IO (Int, Int)
part12 = do
  cs <- components
  let bs = S.toList $ getBridges (S.fromList cs) 0 [] S.empty
  return
    ( maximum $ strength <$> bs,
      strength $ maximumBy (comparing $ (,) <$> length <*> strength) bs
    )
