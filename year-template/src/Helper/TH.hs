module Helper.TH where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Helper.Tracers
import Helper.Util
  ( exampleInputNPath,
    exampleInputPath,
    inputPath,
  )
import Language.Haskell.TH
  ( Exp (AppE, ListE, LitE, TupE, VarE),
    Lit (IntegerL),
    Q,
    mkName,
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

exampleInput :: Int -> Q Exp
exampleInput day = do
  path <- makeRelativeToProject (exampleInputPath day)
  AppE (VarE 'decodeUtf8) <$> embedFile path

exampleInputN :: Int -> Int -> Q Exp
exampleInputN day n = do
  path <- makeRelativeToProject (exampleInputNPath day n)
  AppE (VarE 'decodeUtf8) <$> embedFile path
