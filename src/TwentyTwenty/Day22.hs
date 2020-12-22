module TwentyTwenty.Day22 where

import qualified Data.Foldable as F
import Data.List.Split (splitOn)
import qualified Data.Sequence as SQ
import qualified Data.Set as S

inputPath :: String
inputPath = "input/2020/22.txt"

type Hand = SQ.Seq Int

readInput :: IO (Hand, Hand)
readInput = do
  playerLs <- splitOn [""] . lines <$> readFile inputPath
  let [h1, h2] = fmap read . drop 1 <$> playerLs
  return (SQ.fromList h1, SQ.fromList h2)

play :: Hand -> Hand -> Hand
play h1 SQ.Empty = h1
play SQ.Empty h2 = h2
play (x1 SQ.:<| xs1) (x2 SQ.:<| xs2)
  | x1 < x2 = play xs1 (xs2 SQ.>< SQ.fromList [x2, x1])
  | x1 > x2 = play (xs1 SQ.>< SQ.fromList [x1, x2]) xs2

scoreHand :: Hand -> Int
scoreHand = sum . zipWith (*) [1 ..] . reverse . F.toList

part1 :: IO Int
part1 = scoreHand . uncurry play <$> readInput

data Winner = P1 | P2

playRec :: S.Set (Hand, Hand) -> Hand -> Hand -> (Hand, Winner)
playRec _ h1 SQ.Empty = (h1, P1)
playRec _ SQ.Empty h2 = (h2, P2)
playRec states h1@(x1 SQ.:<| xs1) h2@(x2 SQ.:<| xs2)
  | (h1, h2) `S.member` states = (h1, P1)
  | SQ.length xs1 >= x1 && SQ.length xs2 >= x2 =
    case playRec S.empty (SQ.take x1 xs1) (SQ.take x2 xs2) of
      (_, P1) -> p1Win
      (_, P2) -> p2Win
  | x1 > x2 = p1Win
  | x1 < x2 = p2Win
  where
    nextStates = S.insert (h1, h2) states
    p1Win = playRec nextStates (xs1 SQ.>< SQ.fromList [x1, x2]) xs2
    p2Win = playRec nextStates xs1 (xs2 SQ.>< SQ.fromList [x2, x1])

part2 :: IO Int
part2 = scoreHand . fst . uncurry (playRec S.empty) <$> readInput
