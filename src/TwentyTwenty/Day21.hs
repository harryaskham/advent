module TwentyTwenty.Day21 where

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
import Util

inputPath :: String
inputPath = "input/2020/21.txt"

type Ingredient = String

type Allergen = String

data Food = Food (S.Set Ingredient) (S.Set Allergen) deriving (Show)

foods :: GenParser Char () [Food]
foods = do
  fs <- many food
  eof
  return fs
  where
    food = do
      ingredients <- many1 ingredient
      string "(contains "
      allergens <- many1 allergen
      char '\n'
      return $ Food (S.fromList ingredients) (S.fromList allergens)
    ingredient = do
      i <- many1 letter
      (try spaces >> return i) <|> return i
    allergen = do
      a <- many1 letter
      string ", " <|> string ")"
      return a

validAToI :: [Food] -> M.Map Allergen Ingredient -> Bool
validAToI [] _ = True
validAToI (Food is as : fs) aToI =
  S.delete Nothing (S.map (`M.lookup` aToI) as)
    `S.isSubsetOf` S.map Just is
    && validAToI fs aToI

findValidMappings ::
  [Ingredient] ->
  S.Set Allergen ->
  [Food] ->
  M.Map Ingredient Allergen ->
  [M.Map Ingredient Allergen]
findValidMappings [] as fs aToI = [aToI | S.null as && validAToI fs aToI]
findValidMappings (i : is) as fs aToI = if validAToI fs aToI then with ++ without else []
  where
    choose a = findValidMappings is (S.delete a as) fs (M.insert a i aToI)
    with = choose =<< S.toList as
    without = findValidMappings is as fs aToI

numNonAllergyIngredients :: [Food] -> M.Map Allergen Ingredient -> Int
numNonAllergyIngredients fs aToI =
  length $ filter (not . (`S.member` allergyIngredients)) allIngredients
  where
    allergyIngredients = S.fromList $ M.elems aToI
    allIngredients = (\(Food is _) -> S.toList is) =<< fs

solveForAToI :: IO ([Food], M.Map Allergen Ingredient)
solveForAToI = do
  fs <- readWithParser foods <$> readFile inputPath
  let allIngredients = foldl1 S.union ((\(Food is _) -> is) <$> fs)
      allAllergens = foldl1 S.union ((\(Food _ as) -> as) <$> fs)
      aToIs = findValidMappings (S.toList allIngredients) allAllergens fs M.empty
  return (fs, head aToIs)

part1 :: IO Int
part1 = uncurry numNonAllergyIngredients <$> solveForAToI

part2 :: IO String
part2 = intercalate "," . fmap snd . sort . M.toList . snd <$> solveForAToI
