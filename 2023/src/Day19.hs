module Day19 (part1, part2) where

parser :: Parser (Map String [(Map Char Int -> Maybe String, Either (Char, Char, Int, String) String)], [Map Char Int])
parser = (,) <$> (mkMap <$> (many1 (function <* eol) <* eol)) <*> (many1 (part <* eol) <* eof)

function :: Parser (String, [(Map Char Int -> Maybe String, Either (Char, Char, Int, String) String)])
function =
  let terminator = ((const . Just) &&& Right) <$> many1 alphaNum
      conditional =
        do
          k <- oneOf "xmas"
          opC <- oneOf "><"
          let op = mkMap [('>', (>)), ('<', (<))] |! opC
          bound <- number <* char ':'
          next <- many1 alphaNum
          return (\p -> if p |! k `op` bound then Just next else Nothing, Left (k, opC, bound, next))
   in (,) <$> (many1 alphaNum <* char '{') <*> (((try conditional <|> try terminator) `sepBy1` char ',') <* char '}')

part :: Parser (Map Char Int)
part = char '{' *> (mkMap <$> ((,) <$> (anyChar <* char '=') <*> number) `sepBy1` char ',') <* char '}'

apply :: Map String [Map Char Int -> Maybe String] -> [Map Char Int] -> Int
apply fs ps =
  let applyOne "A" p = sum $ snd <$> unMap p
      applyOne "R" _ = 0
      applyOne f p = applyOne (uhead (catMaybes ((fs |! f) <*> pure p))) p
   in sum $ applyOne "in" <$> ps

totalAccepted :: Map String [Either (Char, Char, Int, String) String] -> Int
totalAccepted fs =
  let target (Right s) = s
      target (Left (_, _, _, s)) = s
      paths =
        mkMapWith
          (<>)
          [ (target (ulast path), [(f, reverse path)])
            | (f, conditionals) <- unMap fs,
              path <- inits conditionals,
              not (null path),
              all isLeft (uinit path)
          ]
      invert (Left (c, '<', n, next)) = Left (c, '>', n - 1, next)
      invert (Left (c, '>', n, next)) = Left (c, '<', n + 1, next)
      invert c = c
      expand Nothing _ = 0
      expand (Just xmas) [] = product [b - a + 1 | (a, b) <- snd <$> unMap xmas]
      expand xmas ((Right _) : cs) = expand xmas cs
      expand (Just xmas) (Left (c, opC, bound, _) : cs) =
        let (a, b) = xmas |! c
         in case opC of
              '<' -> if bound <= a then 0 else expand (Just $ xmas |. (c, (a, min b (bound - 1)))) cs
              '>' -> if bound >= b then 0 else expand (Just $ xmas |. (c, (max a (bound + 1), b))) cs
      follow conditions "in" = expand (Just $ mkMap ((,(1, 4000)) <$> "xmas")) conditions
      follow conditions target = sum [follow (conditions <> (p : (invert <$> ps))) f | (f, p : ps) <- paths |! target]
   in follow [] "A"

part1 :: Int
part1 = $(input 19) |- parser & first (fst <$$>) & uncurry apply

part2 :: Int
part2 = $(input 19) |- parser & fst & (snd <$$>) & totalAccepted