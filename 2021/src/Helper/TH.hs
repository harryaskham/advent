module Helper.TH where

import Data.FileEmbed
import Helper.Util
import Language.Haskell.TH

-- Build a function that runs all days, converts results to Text,
-- and returns [(day, part, result)]
runAllDays :: Q Exp
runAllDays =
  return . ListE $
    ( \(d, p) ->
        ( TupE
            [ Just (LitE $ IntegerL d),
              Just (LitE $ IntegerL p),
              Just
                ( AppE
                    (VarE 'show)
                    (VarE (mkName $ concat ["Day", show d, ".part", show p]))
                )
            ]
        )
    )
      <$> [(d, p) | d <- [1 .. 25], p <- [1, 2]]

-- Literal inputs; use TH to embed the input at compile time

input :: Int -> Q Exp
input day = do
  f <- embedFile (inputPath day)
  return $ AppE (VarE 'decodeUtf8) f
