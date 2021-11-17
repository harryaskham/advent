module TwentyNineteen.Day8 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy, traverse_)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Util (input)

pixelsToLayers :: Int -> Int -> [Int] -> [[Int]]
pixelsToLayers width height ps = chunksOf layerSize ps
  where
    layerSize = width * height

numNs :: Int -> [Int] -> Int
numNs n layer = length (filter (== n) layer)

-- Go through ignoring 2s until we hit a 1 or a 0
combinePixel :: [Int] -> Int
combinePixel (2 : xs) = combinePixel xs
combinePixel (1 : _) = 1
combinePixel (0 : _) = 0

stackLayers :: [[Int]] -> [Int]
stackLayers layers = combinePixel <$> stacks
  where
    stacks = [(!! i) <$> layers | i <- [0 .. length (head layers)]]

part12 :: IO ()
part12 = do
  pixels <- fmap digitToInt . head . lines <$> input 2019 8
  let layers = pixelsToLayers 25 6 pixels
      layerWithFewestZeros = minimumBy (comparing $ numNs 0) layers
      stackedLayers = stackLayers layers
      image = chunksOf 25 stackedLayers
  print $ numNs 1 layerWithFewestZeros * numNs 2 layerWithFewestZeros
  traverse_ print image
