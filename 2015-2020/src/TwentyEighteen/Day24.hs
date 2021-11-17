{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TwentyEighteen.Day24 where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.List (foldl', sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (Down))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra (first, second)
import Safe (headMay)
import Text.ParserCombinators.Parsec
  ( GenParser,
    between,
    char,
    choice,
    digit,
    eof,
    letter,
    many1,
    sepBy,
    string,
    try,
    (<|>),
  )
import Util (eol, input, readWithParser)

newtype UnitCount = UnitCount Int deriving (Show, Eq, Ord, Num)

newtype HitPoints = HitPoints Int deriving (Show, Eq, Ord)

data AttackType = Cold | Fire | Bludgeoning | Slashing | Radiation deriving (Show, Eq, Ord)

data Resistance = Immunity [AttackType] | Weakness [AttackType] deriving (Show, Eq, Ord)

newtype Damage = Damage Int deriving (Show, Eq, Ord)

data Attack = Attack Damage AttackType deriving (Show, Eq, Ord)

newtype Initiative = Initiative Int deriving (Show, Ord, Eq)

data Team = Immune | Infection deriving (Show, Eq, Ord)

type GroupId = Int

data Group = Group
  { team :: Team,
    unitCount :: UnitCount,
    hitPoints :: HitPoints,
    resistances :: [Resistance],
    attack :: Attack,
    initiative :: Initiative,
    groupId :: GroupId
  }
  deriving (Show, Eq, Ord)

groups :: GenParser Char () ([Int -> Group], [Int -> Group])
groups = do
  string "Immune System:" >> eol
  immuneGroups <- many1 (group Immune)
  eol
  string "Infection:" >> eol
  infectionGroups <- many1 (group Infection)
  eof
  return (immuneGroups, infectionGroups)
  where
    attackType = do
      at <- many1 letter
      return $ case at of
        "cold" -> Cold
        "fire" -> Fire
        "bludgeoning" -> Bludgeoning
        "slashing" -> Slashing
        "radiation" -> Radiation
    resistances =
      between
        (char '(')
        (string ") ")
        (resistanceClause `sepBy` string "; ")
        <|> return []
    resistanceClause =
      choice
        [ try $ do
            string "immune to "
            Immunity <$> (attackType `sepBy` string ", "),
          try $ do
            string "weak to "
            Weakness <$> (attackType `sepBy` string ", ")
        ]
    group team = do
      unitCount <- UnitCount . read <$> many1 digit
      string " units each with "
      hitPoints <- HitPoints . read <$> many1 digit
      string " hit points "
      rs <- resistances
      string "with an attack that does "
      dmg <- Damage . read <$> many1 digit
      char ' '
      at <- attackType
      string " damage at initiative "
      initiative <- Initiative . read <$> many1 digit
      eol
      return $ Group team unitCount hitPoints rs (Attack dmg at) initiative

effectivePower :: Group -> Int
effectivePower group =
  let (UnitCount uc) = unitCount group
      (Attack (Damage dmg) _) = attack group
   in uc * dmg

weaknessesImmunities :: Group -> ([AttackType], [AttackType])
weaknessesImmunities group =
  first concat . second concat $ foldl' f ([], []) (resistances group)
  where
    f (ws, is) (Weakness at) = (at : ws, is)
    f (ws, is) (Immunity at) = (ws, at : is)

damageDone :: Group -> Group -> Int
damageDone attacker defender
  | team attacker == team defender = error "Same team attack"
  | at `elem` is = 0
  | at `elem` ws = 2 * pow
  | otherwise = pow
  where
    (ws, is) = weaknessesImmunities defender
    (Attack _ at) = attack attacker
    pow = effectivePower attacker

sortGroupsForTargetSelection :: [Group] -> [Group]
sortGroupsForTargetSelection =
  sortOn (Down . ((,) <$> effectivePower <*> initiative))

selectTarget :: Group -> [Group] -> Set Group -> Maybe Group
selectTarget attacker defenders alreadyTargeted =
  case headMay sortedDefenders of
    Nothing -> Nothing
    Just defender ->
      case damageDone attacker defender of
        0 -> Nothing
        _ -> Just defender
  where
    validDefenders = filter (not . (`S.member` alreadyTargeted)) defenders
    sortedDefenders =
      sortOn
        (Down . ((,,) <$> damageDone attacker <*> effectivePower <*> initiative))
        validDefenders

selectTargets :: [Group] -> [Group] -> Map GroupId GroupId
selectTargets immuneGroups infectionGroups =
  fst $ foldl' f (M.empty, S.empty) sortedGroups
  where
    sortedGroups = sortGroupsForTargetSelection (immuneGroups ++ infectionGroups)
    f (targetMap, alreadyTargeted) attacker =
      let defenders =
            case team attacker of
              Immune -> infectionGroups
              Infection -> immuneGroups
          target = selectTarget attacker defenders alreadyTargeted
       in case target of
            Nothing -> (targetMap, alreadyTargeted)
            Just t ->
              ( M.insert (groupId attacker) (groupId t) targetMap,
                S.insert t alreadyTargeted
              )

sortGroupsForAttack :: [Group] -> [Group]
sortGroupsForAttack = sortOn (Down . initiative)

performAttacks :: Map GroupId Group -> Map GroupId GroupId -> Map GroupId Group
performAttacks groups targetMap =
  foldl' (performAttack targetMap) groups sortedAttackerIds
  where
    sortedAttackerIds = groupId <$> sortGroupsForAttack (M.elems groups)

performAttack :: Map GroupId GroupId -> Map GroupId Group -> GroupId -> Map GroupId Group
performAttack targetMap groups attackerId =
  case M.lookup attackerId groups of
    Nothing -> groups
    Just attacker -> case M.lookup attackerId targetMap of
      Nothing -> groups
      Just tId ->
        let target = groups M.! tId
            dmg = damageDone attacker target
         in case dealDamage dmg target of
              Nothing -> M.delete tId groups
              Just damagedTarget -> M.insert tId damagedTarget groups

unitsLost :: Damage -> Group -> UnitCount
unitsLost (Damage dmg) group =
  let (HitPoints hp) = hitPoints group
   in UnitCount (dmg `div` hp)

dealDamage :: Int -> Group -> Maybe Group
dealDamage dmg group =
  let (UnitCount uc) = unitCount group
      (UnitCount ul) = unitsLost (Damage dmg) group
   in if ul >= uc
        then Nothing
        else Just group {unitCount = UnitCount (uc - ul)}

splitGroups :: Map GroupId Group -> ([Group], [Group])
splitGroups groups =
  ( [g | g <- M.elems groups, team g == Immune],
    [g | g <- M.elems groups, team g == Infection]
  )

fight :: Map GroupId Group -> Map GroupId Group
fight = go S.empty
  where
    go seen groups
      | groups `S.member` seen = groups
      | null immuneGroups || null infectionGroups = groups
      | otherwise =
        go (S.insert groups seen) $ performAttacks groups targetMap
      where
        (immuneGroups, infectionGroups) = splitGroups groups
        targetMap = selectTargets immuneGroups infectionGroups

readGroups :: IO (Map GroupId Group)
readGroups = do
  (immuneGroups', infectionGroups') <-
    readWithParser groups <$> input 2018 24
  let immuneGroups =
        getZipList $ ZipList immuneGroups' <*> ZipList [0 ..]
      infectionGroups =
        getZipList $ ZipList infectionGroups' <*> ZipList [length immuneGroups ..]
  return $ M.fromList [(groupId g, g) | g <- immuneGroups ++ infectionGroups]

part1 :: IO UnitCount
part1 = do
  groups <- readGroups
  return . sum $ unitCount <$> fight groups

boosts :: (Functor f) => Int -> f Group -> f Group
boosts n groups = boost n <$> groups

boost :: Int -> Group -> Group
boost n group
  | team group == Infection = group
  | otherwise = group {attack = Attack (Damage $ dmg + n) at}
  where
    (Attack (Damage dmg) at) = attack group

immuneWins :: Map GroupId Group -> Bool
immuneWins = all ((== Immune) . team) . M.elems

part2 :: IO UnitCount
part2 = do
  groups <- readGroups
  let boostedOutcomes = [fight (boosts b groups) | b <- [1 ..]]
      firstWin = head $ filter immuneWins boostedOutcomes
  return . sum $ unitCount <$> firstWin
