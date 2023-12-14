module Helper.TH where

import Data.Text qualified as T
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Helper.Tracers
import Helper.Grid
import Helper.Util
  ( exampleInputNPath,
    exampleInputPath,
    inputPath,
  )
import Language.Haskell.TH
  ( Exp (AppE, ListE, LitE, TupE, VarE),
    Lit (IntegerL),
    Q,
    mkName
  )

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

gridF :: (Int -> Q Exp) -> Int -> Q Exp
gridF inputFn day = AppE (VarE 'readGrid) <$> inputFn day

grid :: Int -> Q Exp
grid day = AppE (VarE 'readGrid) <$> input day

gridsT :: GridCell a => T.Text -> [Grid a]
gridsT = fmap readGrid . T.splitOn "\n\n"

grids :: Int -> Q Exp
grids day = AppE (VarE 'gridsT) <$> input day
