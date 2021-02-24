{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TwentyNineteen.Day12 where

import Control.Lens
  ( Field1 (_1),
    Field2 (_2),
    Field3 (_3),
    Lens',
    makeLenses,
    view,
    (%~),
    (^.),
  )
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import qualified Data.Map.Strict as M
import Util (unjust)

data Body = Body
  { _bodyId :: Int,
    _position :: (Int, Int, Int),
    _velocity :: (Int, Int, Int)
  }
  deriving (Eq, Show)

makeLenses ''Body

bodies =
  [ Body 0 (-17, 9, -5) (0, 0, 0),
    Body 1 (-1, 7, 13) (0, 0, 0),
    Body 2 (-19, 12, 5) (0, 0, 0),
    Body 3 (-6, -6, -4) (0, 0, 0)
  ]

bodiesMap = M.fromList $ zip (view bodyId <$> bodies) bodies

testBodies =
  [ Body 0 (-1, -0, 2) (0, 0, 0),
    Body 1 (2, -10, -7) (0, 0, 0),
    Body 2 (4, -8, 8) (0, 0, 0),
    Body 3 (3, 5, -1) (0, 0, 0)
  ]

testBodiesMap = M.fromList $ zip (view bodyId <$> testBodies) testBodies

pairGravity :: Lens' (Int, Int, Int) Int -> (Body, Body) -> (Body, Body)
pairGravity _n (b1, b2)
  | b1 ^. position . _n < b2 ^. position . _n =
    (b1 & velocity . _n %~ (+ 1), b2 & velocity . _n %~ subtract 1)
  | b1 ^. position . _n > b2 ^. position . _n =
    (b1 & velocity . _n %~ subtract 1, b2 & velocity . _n %~ (+ 1))
  | otherwise = (b1, b2)

pairGravityX :: (Body, Body) -> (Body, Body)
pairGravityX = pairGravity _1

pairGravityY :: (Body, Body) -> (Body, Body)
pairGravityY = pairGravity _2

pairGravityZ :: (Body, Body) -> (Body, Body)
pairGravityZ = pairGravity _3

applyGravity :: [(Body, Body) -> (Body, Body)] -> M.Map Int Body -> M.Map Int Body
applyGravity axisFns bs = overAxes
  where
    ids = fst <$> M.toList bs
    allIdPairs = [(a, b) | a <- ids, b <- ids, a < b]
    foldFn pairF acc (id1, id2) =
      let b1 = unjust $ M.lookup id1 acc
          b2 = unjust $ M.lookup id2 acc
          (b1', b2') = pairF (b1, b2)
       in M.insert id1 b1' . M.insert id2 b2' $ acc
    overAxes = foldl' (\acc axisFn -> foldl' (foldFn axisFn) acc allIdPairs) bs axisFns

stepBody :: Body -> Body
stepBody body =
  body & position . _1 %~ (+ body ^. velocity . _1)
    & position . _2 %~ (+ body ^. velocity . _2)
    & position . _3 %~ (+ body ^. velocity . _3)

stepSim :: [(Body, Body) -> (Body, Body)] -> M.Map Int Body -> M.Map Int Body
stepSim axisFns bs = stepBody <$> applyGravity axisFns bs

kinetic :: Body -> Int
kinetic body =
  abs (body ^. velocity . _1)
    + abs (body ^. velocity . _2)
    + abs (body ^. velocity . _3)

potential :: Body -> Int
potential body =
  abs (body ^. position . _1)
    + abs (body ^. position . _2)
    + abs (body ^. position . _3)

energy :: Body -> Int
energy = (*) <$> kinetic <*> potential

stepN :: Int -> M.Map Int Body -> M.Map Int Body
stepN 0 bs = bs
stepN n bs = stepN (n -1) $ stepSim [pairGravityX, pairGravityY, pairGravityZ] bs

stepUntilReturn :: [(Body, Body) -> (Body, Body)] -> M.Map Int Body -> Int
stepUntilReturn axisFns orig = go 0 orig
  where
    go :: Int -> M.Map Int Body -> Int
    go n bs = if n > 0 && orig == bs then n else go (n + 1) $ stepSim axisFns bs

part1 :: Int
part1 = sum $ energy <$> (snd <$> M.toList (stepN 1000 bodiesMap))

part2 :: Int
part2 =
  foldl1
    lcm
    [ stepUntilReturn [pairGravityX] bodiesMap,
      stepUntilReturn [pairGravityY] bodiesMap,
      stepUntilReturn [pairGravityZ] bodiesMap
    ]
