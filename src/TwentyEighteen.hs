module TwentyEighteen where

day1_1 :: IO Int
day1_1 = do
  content <- readFile "input/2018/1_1.txt"
  return $ sum (parseLine <$> lines content)
    where
      parseLine :: String -> Int
      parseLine (sign:number) = case sign of
                                  '+' -> read number
                                  '-' -> -1 * read number
