module TwentyFifteen.Day4 where

import Util (md5String)

mine :: Int -> Int
mine n =
  head
    [ x
      | x <- [1 ..],
        let hash = md5String ("iwrupvqb" ++ show x)
            leading = take n hash,
        leading == replicate n '0'
    ]

part1 :: Int
part1 = mine 5

part2 :: Int
part2 = mine 6
