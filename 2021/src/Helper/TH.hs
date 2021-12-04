module Helper.TH where

import Data.FileEmbed
import Helper.Util
import Language.Haskell.TH

-- Build a function that runs all days, converts results to Text,
-- and returns IO [(day, part, result)]
runAllDays :: Q Exp
runAllDays =
  return . AppE (VarE 'sequence) . ListE $
    ( \(d, p) ->
        AppE
          ( AppE
              (VarE 'fmap)
              ( TupE
                  [ Just (LitE $ IntegerL d),
                    Just (LitE $ IntegerL p),
                    Nothing
                  ]
              )
          )
          ( AppE
              (AppE (VarE 'fmap) (VarE 'show))
              (VarE (mkName $ concat ["Day", show d, ".part", show p]))
          )
    )
      <$> [(d, p) | d <- [1 .. 25], p <- [1, 2]]

-- Literal inputs; use TH to embed the input at compile time

inputL :: Int -> Q Exp
inputL day = do
  f <- embedFile (input day)
  return $ AppE (VarE 'decodeUtf8) f
