module TwentyFifteen.Day21 where

import Data.Monoid (Sum (Sum, getSum))
import Data.Tuple.Extra (fst3)

type Item = (Int, Int, Int)

weapons :: [Item]
weapons =
  [ (,,) 8 4 0,
    (,,) 10 5 0,
    (,,) 25 6 0,
    (,,) 40 7 0,
    (,,) 74 8 0
  ]

armors :: [Item]
armors =
  [ (,,) 13 0 1,
    (,,) 31 0 2,
    (,,) 53 0 3,
    (,,) 75 0 4,
    (,,) 102 0 5
  ]

rings :: [Item]
rings =
  [ (,,) 25 1 0,
    (,,) 50 2 0,
    (,,) 100 3 0,
    (,,) 20 0 1,
    (,,) 40 0 2,
    (,,) 80 0 3
  ]

player :: [Item] -> (Int, Int, Int)
player items =
  (\(d, a) -> (100, getSum d, getSum a))
    . foldl1 (<>)
    $ [(Sum damage, Sum armor) | (_, damage, armor) <- items]

playerWins :: (Int, Int, Int) -> Bool
playerWins player = go player (104, 8, 1) True
  where
    go (pHP, pDamage, pArmor) (bHP, bDamage, bArmor) pTurn
      | pHP <= 0 = False
      | bHP <= 0 = True
      | pTurn =
        let dmg = maximum [pDamage - bArmor, 1]
         in go (pHP, pDamage, pArmor) (bHP - dmg, bDamage, bArmor) False
      | otherwise =
        let dmg = maximum [bDamage - pArmor, 1]
         in go (pHP - dmg, pDamage, pArmor) (bHP, bDamage, bArmor) True

part1 :: Int
part1 =
  minimum
    [ sum $ fst3 <$> items
      | w <- weapons,
        a <- [] : (pure <$> armors),
        rs <- [] : ([[r1, r2] | r1 <- rings, r2 <- rings, r1 /= r2] ++ (pure <$> rings)),
        let items = w : a ++ rs,
        playerWins (player items)
    ]

part2 :: Int
part2 =
  maximum
    [ sum $ fst3 <$> items
      | w <- weapons,
        a <- [] : (pure <$> armors),
        rs <- [] : ([[r1, r2] | r1 <- rings, r2 <- rings, r1 /= r2] ++ (pure <$> rings)),
        let items = w : a ++ rs,
        not $ playerWins (player items)
    ]
