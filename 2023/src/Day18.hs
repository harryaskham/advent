module Day18 (part1, part2) where

solve :: (((Dir2, Int), (Dir2, Int)) -> (Dir2, Int)) -> Int
solve f =
  $(input 18)
    |- ( f
           <$$> ( many
                    ( (,)
                        <$> ((,) <$> (udlrToDir2 <$> anyChar <* spaces) <*> (number <* spaces))
                        <*> ( swap
                                <$> ( string "(#"
                                        *> ( (,)
                                               <$> (uread . ("0x" <>) <$> count 5 hexDigit)
                                               <*> (([DirRight, DirDown, DirLeft, DirUp] !!) <$> number)
                                           )
                                        <* (char ')' >> eol)
                                    )
                            )
                    )
                    <* eof
                )
       )
    & pick (0, 0) 0 0

pick :: (Int, Int) -> Int -> Int -> [(Dir2, Int)] -> Int
pick _ p a [] = a + (p `div` 2) + 1
pick (x, y) p a ((d, l) : rest) =
  let (x', y') = move d l (x, y)
   in pick (x', y') (p + l) (a + x' * (y' - y)) rest

part1 :: Int
part1 = solve fst

part2 :: Int
part2 = solve snd