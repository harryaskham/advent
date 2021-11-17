module TwentySixteen.Day21 where

import Data.Char (digitToInt)
import qualified Data.Foldable as F
import Data.List (foldl')
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Text.ParserCombinators.Parsec
  ( GenParser,
    choice,
    digit,
    eof,
    letter,
    many,
    noneOf,
    string,
    try,
  )
import Util (eol, input, readWithParser)

data Transformation
  = SwapPosition Int Int
  | SwapLetter Char Char
  | RotateLeft Int
  | RotateRight Int
  | RotateLetter Char
  | ReversePositions Int Int
  | MovePosition Int Int

transformations :: GenParser Char () [Transformation]
transformations = do
  ts <- many transformation
  eof
  return ts
  where
    transformation = do
      t <-
        choice
          ( try
              <$> [ swapPosition,
                    swapLetter,
                    rotateLeft,
                    rotateRight,
                    rotateLetter,
                    reversePositions,
                    movePosition
                  ]
          )
      eol
      return t
    dig = digitToInt <$> digit
    swapPosition = do
      string "swap position "
      p1 <- dig
      string " with position "
      p2 <- dig
      return $ SwapPosition p1 p2
    swapLetter = do
      string "swap letter "
      l1 <- letter
      string " with letter "
      l2 <- letter
      return $ SwapLetter l1 l2
    rotateLeft = do
      string "rotate left "
      steps <- dig
      many (noneOf "\n")
      return $ RotateLeft steps
    rotateRight = do
      string "rotate right "
      steps <- dig
      many (noneOf "\n")
      return $ RotateRight steps
    rotateLetter = do
      string "rotate based on position of letter "
      l <- letter
      return $ RotateLetter l
    reversePositions = do
      string "reverse positions "
      p1 <- dig
      string " through "
      p2 <- dig
      return $ ReversePositions p1 p2
    movePosition = do
      string "move position "
      p1 <- dig
      string " to position "
      p2 <- dig
      return $ MovePosition p1 p2

transform :: Seq Char -> Transformation -> Seq Char
transform s (SwapPosition p1 p2) =
  SQ.update p1 (s `SQ.index` p2) . SQ.update p2 (s `SQ.index` p1) $ s
transform s (SwapLetter c1 c2) =
  let Just p1 = SQ.elemIndexL c1 s
      Just p2 = SQ.elemIndexL c2 s
   in transform s (SwapPosition p1 p2)
transform s (RotateLeft steps) =
  let (a, b) = SQ.splitAt steps s in b SQ.>< a
transform s (RotateRight steps) =
  let (a, b) = SQ.splitAt ((length s - steps + length s) `mod` length s) s in b SQ.>< a
transform s (RotateLetter c) =
  let Just p = SQ.elemIndexL c s
      steps = p + 1 + (if p >= 4 then 1 else 0)
   in transform s (RotateRight steps)
transform s (ReversePositions p1 p2) =
  let (a, b') = SQ.splitAt p1 s
      (b, c) = SQ.splitAt (p2 - p1 + 1) b'
   in a SQ.>< SQ.reverse b SQ.>< c
transform s (MovePosition p1 p2) =
  let c = s `SQ.index` p1
   in SQ.insertAt p2 c . SQ.deleteAt p1 $ s

part1 :: IO String
part1 = do
  ts <- readWithParser transformations <$> input 2016 21
  return $ F.toList $ foldl' transform (SQ.fromList "abcdefgh") ts

reverseTransform :: Transformation -> Seq Char -> Seq Char
reverseTransform t@(SwapPosition _ _) s = transform s t
reverseTransform t@(SwapLetter _ _) s = transform s t
reverseTransform (RotateLeft steps) s = transform s (RotateRight steps)
reverseTransform (RotateRight steps) s = transform s (RotateLeft steps)
reverseTransform (RotateLetter c) s =
  head
    [ s'
      | steps <- [0 .. length s],
        let s' = transform s (RotateLeft steps),
        transform s' (RotateLetter c) == s
    ]
reverseTransform t@(ReversePositions _ _) s = transform s t
reverseTransform (MovePosition p1 p2) s = transform s (MovePosition p2 p1)

part2 :: IO String
part2 = do
  ts <- readWithParser transformations <$> input 2016 21
  return $ F.toList $ foldr reverseTransform (SQ.fromList "fbgdceah") ts
