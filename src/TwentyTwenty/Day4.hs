module TwentyTwenty.Day4 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

inputPath :: String
inputPath = "input/2020/4.txt"

type Passport = M.Map String String

linesToPassport :: [String] -> Passport
linesToPassport ls =
  let kvPairs = splitOn ":" <$> (concat $ splitOn " " <$> ls)
   in M.fromList ((\[a, b] -> (a, b)) <$> kvPairs)

isValid :: Passport -> Bool
isValid passport =
  all
    (`M.member` passport)
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 :: IO Int
part1 = do
  ls <- lines <$> readFile inputPath
  let passports = linesToPassport <$> splitOn [""] ls
  return $ length $ filter isValid passports

unit :: String -> String
unit s = drop (length s - 2) s

value :: String -> String
value s = take (length s - 2) s

data ErrorType = Missing | Invalid deriving (Show)

data ValidationError
  = BYRError ErrorType
  | IYRError ErrorType
  | EYRError ErrorType
  | HGTError ErrorType
  | HCLError ErrorType
  | ECLError ErrorType
  | PIDError ErrorType
  deriving (Show)

validate :: Passport -> [ValidationError]
validate p =
  catMaybes
    [ case M.lookup "byr" p of
        Nothing -> Just $ BYRError Missing
        Just byr' ->
          let byr = read byr'
           in if byr >= 1920 && byr <= 2002
                then Nothing
                else Just $ BYRError Invalid,
      case M.lookup "iyr" p of
        Nothing -> Just $ EYRError Missing
        Just iyr' ->
          let iyr = read iyr'
           in if iyr >= 2010 && iyr <= 2020
                then Nothing
                else Just $ IYRError Invalid,
      case M.lookup "eyr" p of
        Nothing -> Just $ EYRError Missing
        Just eyr' ->
          let eyr = read eyr'
           in if eyr >= 2020 && eyr <= 2030
                then Nothing
                else Just $ EYRError Invalid,
      case M.lookup "hgt" p of
        Nothing -> Just $ HGTError Missing
        Just hgt ->
          if length hgt <= 2 || not (unit hgt == "cm" || unit hgt == "in")
            then Just $ HGTError Invalid
            else
              let hgt' = read (value hgt)
               in case unit hgt of
                    "cm" ->
                      if hgt' >= 150 && hgt' <= 193
                        then Nothing
                        else Just $ HGTError Invalid
                    "in" ->
                      if hgt' >= 59 && hgt' <= 76
                        then Nothing
                        else Just $ HGTError Invalid,
      case M.lookup "hcl" p of
        Nothing -> Just $ HCLError Missing
        Just hcl ->
          if ( length hcl == 7
                 && head hcl == '#'
                 && all (`S.member` S.fromList "abcdef0123456789") (drop 1 hcl)
             )
            then Nothing
            else Just $ HCLError Invalid,
      case M.lookup "ecl" p of
        Nothing -> Just $ ECLError Missing
        Just ecl ->
          if ecl `S.member` S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            then Nothing
            else Just $ ECLError Invalid,
      case M.lookup "pid" p of
        Nothing -> Just $ PIDError Missing
        Just pid ->
          if ( length pid == 9
                 && all (`S.member` S.fromList "0123456789") pid
             )
            then Nothing
            else Just $ PIDError Invalid
    ]

part2 :: IO Int
part2 = do
  ls <- lines <$> readFile inputPath
  let passports = linesToPassport <$> splitOn [""] ls
      validationErrors = validate <$> passports
  return $ length $ filter null validationErrors
