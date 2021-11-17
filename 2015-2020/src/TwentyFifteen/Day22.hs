module TwentyFifteen.Day22 where

import Data.Maybe (isJust)
import qualified Data.Sequence as SQ

data Game = Game
  { pHp :: Int,
    pMana :: Int,
    bHp :: Int,
    bDmg :: Int,
    shield :: Maybe Int,
    poison :: Maybe Int,
    recharge :: Maybe Int
  }
  deriving (Show)

mkGame :: Game
mkGame =
  Game
    { pHp = 50,
      pMana = 500,
      bHp = 55,
      bDmg = 8,
      shield = Nothing,
      poison = Nothing,
      recharge = Nothing
    }

tickEffects :: Game -> Game
tickEffects game =
  game
    { shield = tick (shield game),
      poison = tick (poison game),
      recharge = tick (recharge game)
    }
  where
    tick Nothing = Nothing
    tick (Just 0) = Nothing
    tick (Just x) = Just (x - 1)

applyEffects :: Game -> Game
applyEffects game
  | pHp game <= 0 || bHp game <= 0 = game
  | otherwise =
    game
      { bHp = case poison game of
          Nothing -> bHp game
          Just _ -> bHp game - 3,
        pMana = case recharge game of
          Nothing -> pMana game
          Just _ -> pMana game + 101
      }

bossHit :: Game -> Game
bossHit game
  | bHp game <= 0 = game
  | otherwise =
    game {pHp = pHp game - dmg}
  where
    dmg = case shield game of
      Nothing -> bDmg game
      Just _ -> maximum [bDmg game - 7, 1]

data SpellType
  = MagicMissile
  | Drain
  | Shield
  | Poison
  | Recharge
  deriving (Show)

data Spell = Spell Int SpellType deriving (Show)

runSpell' :: Spell -> Game -> Game
runSpell' (Spell cost MagicMissile) game =
  game {pMana = pMana game - cost, bHp = bHp game - 4}
runSpell' (Spell cost Drain) game =
  game {pMana = pMana game - cost, bHp = bHp game - 2, pHp = pHp game + 2}
runSpell' (Spell cost Shield) game =
  game {pMana = pMana game - cost, shield = Just 6}
runSpell' (Spell cost Poison) game =
  game {pMana = pMana game - cost, poison = Just 6}
runSpell' (Spell cost Recharge) game =
  game {pMana = pMana game - cost, recharge = Just 5}

runSpell :: Spell -> Game -> Game
runSpell spell game
  | pHp game <= 0 = game
  | otherwise = runSpell' spell game

spellCost :: Spell -> Int
spellCost (Spell c _) = c

getValidSpells :: Game -> [Spell]
getValidSpells game =
  filter ((pMana game >=) . spellCost) $
    ( case poison game of
        Nothing -> [Spell 173 Poison]
        Just 0 -> [Spell 173 Poison]
        Just _ -> []
    )
      ++ ( case shield game of
             Nothing -> [Spell 113 Shield]
             Just 0 -> [Spell 113 Shield]
             Just _ -> []
         )
      ++ ( case recharge game of
             Nothing -> [Spell 229 Recharge]
             Just 0 -> [Spell 229 Recharge]
             Just _ -> []
         )
      ++ [Spell 53 MagicMissile, Spell 73 Drain]

runGame :: (Game -> Game) -> Game -> Maybe Int
runGame modifier game = go (SQ.singleton (game, 0, True)) Nothing
  where
    go SQ.Empty best = best
    go ((game, manaSpent, pTurn) SQ.:<| queue) best
      | isJust best && Just manaSpent >= best = go queue best
      | pHp game <= 0 = endWithLoss
      | bHp game <= 0 = endWithWin
      | pTurn && null validSpells = endWithLoss
      | otherwise = go (nextStates SQ.>< queue) best
      where
        endWithLoss = go queue best
        nextBest = case best of
          Nothing -> Just manaSpent
          Just best -> Just $ minimum [manaSpent, best]
        endWithWin = go queue nextBest
        validSpells = getValidSpells game
        nextStates =
          SQ.fromList $
            if pTurn
              then
                [ (tickEffects . runSpell spell . applyEffects . modifier $ game, manaSpent + cost, False)
                  | spell@(Spell cost _) <- validSpells
                ]
              else [(tickEffects . bossHit . applyEffects $ game, manaSpent, True)]

part1 :: Maybe Int
part1 = runGame id mkGame

part2 :: Maybe Int
part2 = runGame (\game -> game {pHp = pHp game - 1}) mkGame
