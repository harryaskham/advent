module Helper.TH where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text qualified as T
import Helper.Grid
import Helper.Tracers
import Helper.Util
  ( exampleInputNPath,
    exampleInputPath,
    inputPath,
    parseWith,
  )
import Language.Haskell.TH
  ( Exp (AppE, ListE, LitE, TupE, VarE),
    Lit (IntegerL),
    Q,
    mkName,
  )
import Text.ParserCombinators.Parsec

-- Build a function that runs all days, converts results to Text,
-- and returns [(day, part, result)]
runAllDays :: Q Exp
runAllDays =
  return . ListE $
    ( \(d, p) ->
        TupE
          [ Just (LitE $ IntegerL d),
            Just (LitE $ IntegerL p),
            Just
              ( AppE
                  (VarE 'show)
                  (VarE (mkName $ concat ["Day", show d, ".part", show p]))
              )
          ]
    )
      <$> [(d, p) | d <- [1 .. 25], p <- [1, 2]]

-- Literal inputs; use TH to embed the input at compile time

input :: Int -> Q Exp
input day = do
  path <- makeRelativeToProject (inputPath day)
  AppE (VarE 'decodeUtf8) <$> embedFile path

inputS :: Int -> Q Exp
inputS day = AppE (VarE 'T.unpack) <$> input day

exampleInput :: Int -> Q Exp
exampleInput day = do
  path <- makeRelativeToProject (exampleInputPath day)
  AppE (VarE 'decodeUtf8) <$> embedFile path

exampleInputN :: Int -> Int -> Q Exp
exampleInputN day n = do
  path <- makeRelativeToProject (exampleInputNPath day n)
  AppE (VarE 'decodeUtf8) <$> embedFile path

grid :: (Int -> Q Exp) -> Int -> Q Exp
grid inputFn day = AppE (VarE 'readGrid) <$> inputFn day

gridsT :: (GridCell a, Griddable Identity g) => T.Text -> [g a]
gridsT = fmap readGrid . T.splitOn "\n\n"

grids :: (Int -> Q Exp) -> Int -> Q Exp
grids inputFn day = AppE (VarE 'gridsT) <$> inputFn day