module Day8 (part1, part2) where

parser :: Parser ([Char], Map String (String, String))
parser =
  let name = count 3 alphaNum
   in (,)
        <$> (many1 (oneOf "LR") <* eol <* eol)
        <*> ( mkMap
                <$> many1
                  ( (,)
                      <$> (name <* string " = (")
                      <*> ( (,)
                              <$> (name <* string ", ")
                              <*> (name <* string ")" <* eol)
                          )
                  )
                <* eof
            )

travel :: (String -> Bool) -> String -> String -> Map String (String, String) -> [Int]
travel p s lr' g = go (cycle lr') 0 s
  where
    go (d : lr) n c
      | p c = n : next
      | otherwise = next
      where
        next = case d of
          'L' -> go lr (n + 1) (fst $ g |! c)
          'R' -> go lr (n + 1) (snd $ g |! c)

part1 :: Int
part1 =
  $(input 8)
    & parseWith parser
    & uncurry (travel (== "ZZZ") "AAA")
    & uhead

part2 :: Int
part2 =
  $(input 8)
    & parseWith parser
    & ( \(lr, g) ->
          [ travel ((!! 2) >>> (== 'Z')) s lr g
            | s <- [s | (s, _) <- unMap g, s !! 2 == 'A']
          ]
      )
    & fmap uhead
    & foldl1 lcm
