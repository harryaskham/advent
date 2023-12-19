module Day19 (part1, part2) where

function :: Parser (String, [Map Char Int -> Maybe String])
function =
  let terminator = const . Just <$> many1 alphaNum
      conditional =
        do
          k <- oneOf "xmas"
          op <- (mkMap [('>', (>)), ('<', (<))] |!) <$> oneOf "><"
          bound <- number <* char ':'
          next <- many1 alphaNum
          return (\p -> if p |! k `op` bound then Just next else Nothing)
   in (,) <$> (many1 alphaNum <* char '{') <*> (((try conditional <|> try terminator) `sepBy1` char ',') <* char '}')

part :: Parser (Map Char Int)
part = char '{' *> (mkMap <$> ((,) <$> (anyChar <* char '=') <*> number) `sepBy1` char ',') <* char '}'

apply :: Map String [Map Char Int -> Maybe String] -> [Map Char Int] -> Int
apply namespace ps =
  let applyOne "A" p = sum $ snd <$> unMap p
      applyOne "R" _ = 0
      applyOne f p = applyOne (uhead (catMaybes ((namespace |! f) <*> pure p))) p
   in sum $ applyOne "in" <$> ps

parser :: Parser (Map String [Map Char Int -> Maybe String], [Map Char Int])
parser = (,) <$> (mkMap <$> (many1 (function <* eol) <* eol)) <*> (many1 (part <* eol) <* eof)

part1 :: Int
part1 = $(input 19) |- parser & uncurry apply

part2 :: Int
part2 = $(input 19) |- parser & uncurry apply