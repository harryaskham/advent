module TwentySixteen.Day10 where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Sequence as SQ
import Text.ParserCombinators.Parsec
  ( GenParser,
    choice,
    digit,
    eof,
    many,
    many1,
    string,
    try,
    (<|>),
  )
import Util (adjustWithDefault, eol, readWithParser)

inputPath :: String
inputPath = "input/2016/10.txt"

data Id = BotId Int | OutputId Int deriving (Show)

toBotId :: Id -> Maybe Int
toBotId (BotId x) = Just x
toBotId (OutputId _) = Nothing

data GiveLowHigh = GiveLowHigh Id Id deriving (Show)

factory :: GenParser Char () (M.Map Int [Int], M.Map Int GiveLowHigh)
factory = do
  initsAndIs <- many (try initValue <|> try giveInstruction)
  eof
  let (initBots, updateOps) = unzip initsAndIs
  return (M.fromListWith (++) $ catMaybes initBots, M.fromList $ catMaybes updateOps)
  where
    initValue = do
      string "value "
      v <- read <$> many1 digit
      string " goes to bot "
      bId <- read <$> many1 digit
      eol
      return (Just (bId, [v]), Nothing)
    giveInstruction = do
      string "bot "
      bId <- read <$> many1 digit
      string " gives low to "
      lId <- botOutputId
      string " and high to "
      hId <- botOutputId
      eol
      return (Nothing, Just (bId, GiveLowHigh lId hId))
    botOutputId =
      choice
        [ do
            string "bot "
            bId <- read <$> many1 digit
            return $ BotId bId,
          do
            string "output "
            oId <- read <$> many1 digit
            return $ OutputId oId
        ]

runFrom :: SQ.Seq Int -> M.Map Int [Int] -> M.Map Int GiveLowHigh -> M.Map Int [Int] -> Int -> (Int, Int)
runFrom q bots ops outputs p1Ans =
  case SQ.viewl q of
    SQ.EmptyL -> (p1Ans, product ((outputs M.!) =<< [0, 1, 2]))
    bId SQ.:< rest ->
      case M.findWithDefault [] bId bots of
        [] -> runFrom rest bots ops outputs p1Ans
        [_] -> runFrom rest bots ops outputs p1Ans
        ab ->
          let (max, min) = (maximum ab, minimum ab)
              (GiveLowHigh lowId highId) = ops M.! bId
              updateLow (b, o) = case lowId of
                BotId lowBotId -> (adjustWithDefault [] (min :) lowBotId b, o)
                OutputId lowOId -> (b, adjustWithDefault [] (min :) lowOId o)
              updateHigh (b, o) = case highId of
                BotId lowBotId -> (adjustWithDefault [] (max :) lowBotId b, o)
                OutputId lowOId -> (b, adjustWithDefault [] (max :) lowOId o)
              zeroOut (b, o) = (M.insert bId [] b, o)
              (newBots, newOutputs) = updateLow . updateHigh . zeroOut $ (bots, outputs)
              newQ = (rest SQ.>< SQ.fromList (mapMaybe toBotId [lowId, highId]))
           in case (max, min) of
                (61, 17) -> runFrom newQ newBots ops newOutputs bId
                _ -> runFrom newQ newBots ops newOutputs p1Ans

part12 :: IO (Int, Int)
part12 = do
  (bots, ops) <- readWithParser factory <$> readFile inputPath
  let start = head . M.keys . M.filter ((== 2) . length) $ bots
  return $ runFrom (SQ.singleton start) bots ops M.empty 0
