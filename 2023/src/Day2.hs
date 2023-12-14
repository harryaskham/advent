module Day2 (part1, part2) where

data Color = Red Int | Blue Int | Green Int

data Game = Game {index :: Int, cubes :: [[Color]]}

games :: Parser [Game]
games = game `sepBy1` eol <* eof
  where
    color = do
      n <- number <* string " "
      choice [Red n <$ string "red", Green n <$ string "green", Blue n <$ string "blue"]
    game = do
      i <- string "Game " *> number <* string ": "
      Game i <$> (color `sepBy1` string ", ") `sepBy1` string "; "

possibleGame :: Game -> Bool
possibleGame = all (all possibleColor) . cubes
  where
    possibleColor (Red i) = i <= 12
    possibleColor (Green i) = i <= 13
    possibleColor (Blue i) = i <= 14

power :: [Color] -> Int
power colors = go colors 0 0 0
  where
    go [] r g b = r * g * b
    go ((Red i) : cs) r g b = go cs (max i r) g b
    go ((Green i) : cs) r g b = go cs r (max i g) b
    go ((Blue i) : cs) r g b = go cs r g (max i b)

part1 :: Int
part1 = $(input 2) |- games & filter possibleGame & fmap index & sum

part2 :: Int
part2 = $(input 2) |- games & fmap (power . mconcat . cubes) & sum
