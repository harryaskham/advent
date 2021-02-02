{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module TwentyEighteen.Day23 where

import Control.Monad (when)
import Coord (Coord3, manhattan3)
import Data.Algorithm.MaximalCliques (getMaximalCliques)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.List.Extra (maximumOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (Down))
import Debug.Trace (traceShow)
import Numeric.Backprop (auto, evalBP, gradBP, sequenceVar)
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.Adam as ADAM
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec
  ( GenParser,
    between,
    char,
    eof,
    many,
    many1,
    oneOf,
    sepBy,
    string,
  )
import Util (eol, input, readWithParser)

data Nanobot = Nanobot Coord3 Int deriving (Eq, Ord, Show)

radius :: Nanobot -> Int
radius (Nanobot _ r) = r

pos :: Nanobot -> Coord3
pos (Nanobot pos _) = pos

nanobots :: GenParser Char () [Nanobot]
nanobots = do
  ns <- many nanobot
  eof
  return ns
  where
    val = read <$> many1 (oneOf "-0123456789")
    nanobot = do
      [x, y, z] <- between (string "pos=<") (string ">, r=") (val `sepBy` char ',')
      r <- val
      eol
      return $ Nanobot (x, y, z) r

part1 :: IO Int
part1 = do
  bots <- readWithParser nanobots <$> input 2018 23
  let maxBot = maximumOn radius bots
      inRange bot = manhattan3 (pos bot) (pos maxBot) <= radius maxBot
  return . length . filter inRange $ bots

seenBy :: Coord3 -> Nanobot -> Bool
seenBy pos (Nanobot botPos r) = manhattan3 pos botPos <= r

botsOverlap :: Nanobot -> Nanobot -> Bool
botsOverlap (Nanobot pos1 r1) (Nanobot pos2 r2) = manhattan3 pos1 pos2 <= r1 + r2

-- Solve part two by SGD over 3D space looking for the single point of maximal intersection.
part2 :: IO Int
part2 = do
  -- First identify the maximal clique of overlapping bots - turns out to be 982 of them.
  bots <- readWithParser nanobots <$> input 2018 23
  let cliques = getMaximalCliques botsOverlap bots
      clique = head . sortOn (Down . length) $ cliques
  --print $ manhattan3 (0, 0, 0) (24764201, 11698589, 11739489)
  -- Run gradient descent using the SGD and Backprop libraries for auto-differentiation.
  -- Set up two IORef trackers to keep step count and the most-covered location.
  step <- newIORef 0
  best <- newIORef (0, (0, 0, 0))
  -- Convert bots to more gradient-convenient BVars.
  -- Only consider the bots in the clique
  let xyzrs =
        ( \(Nanobot (x, y, z) r) ->
            ( auto $ fromIntegral x,
              auto $ fromIntegral y,
              auto $ fromIntegral z,
              auto $ fromIntegral r
            )
        )
          <$> clique
      -- A single bot's loss is zero if we're in its range, otherwise scales with the manhattan distance.
      botLoss x y z (bx, by, bz, r) =
        let d = abs (bx - x) + abs (by - y) + abs (bz - z)
         in if d <= r then 0.0 else d
      -- The combined loss function is the distance to all bots, plus tries to minimise distance to origin.
      -- This should allow us to find the minimal overlap, but in my example,
      -- only a single coordinate was visible to all bots.
      botLosses x y z = sum (botLoss x y z <$> (0, 0, 0, 0) : xyzrs)
      -- A helper to compute the number seen for a given coordinate so we know when we're fully covered.
      seenByPs x y z = length $ filter ((round x, round y, round z) `seenBy`) bots
      -- Finally construct the loss using ViewPatterns to enable vector inputs.
      loss (sequenceVar -> [x, y, z]) = botLosses x y z
      -- Use Backprop's gradBP to autodifferentiate the loss
      deriv xyzM =
        let [x, y, z] = (xyzM M.!) <$> "xyz"
            [x', y', z'] = gradBP loss [x, y, z]
            numSeen = seenByPs x y z
            -- Update step counters / best tracker.
            (s, b, bPos) = unsafePerformIO $ do
              step' <- readIORef step
              (b, bPos) <- readIORef best
              modifyIORef' step (+ 1)
              when (numSeen > b) (writeIORef best (numSeen, (round x, round y, round z)))
              return (step', b, bPos)
         in -- Do some per-step logging.
            traceShow
              ( s,
                b,
                bPos,
                numSeen,
                manhattan3 (0, 0, 0) (round x, round y, round z),
                (round x, round y, round z),
                round $ evalBP loss [x, y, z]
              )
              -- Return an updated parameter map
              $ M.fromList (zip "xyz" [x', y', z'])
      -- Run the SGD representing (x,y,z) as a Map, which implements SGD's ParamSet
      -- These parameters happen to converge in around 108,000 steps
      result :: Map Char Double
      result =
        SGD.run
          ( SGD.adam
              ( ADAM.Config
                  { ADAM.alpha0 = 100000,
                    ADAM.tau = 100,
                    ADAM.beta1 = 0.9,
                    ADAM.beta2 = 0.999,
                    ADAM.eps = 1.0e-8
                  }
              )
              id
          )
          -- Construct 'dataset' of 110000 steps of grad updates
          (replicate 110000 deriv)
          -- Start search at the origin
          (M.fromList (zip "xyz" [0, 0, 0]))
  -- Print the final result - unlikely to be the best as it won't have fully converged.
  print $ (result M.!) <$> "xyz"
  -- Instead, we found the best position, so we take its distance to origin.
  (_, bestPos) <- readIORef best
  return $ manhattan3 (0, 0, 0) bestPos
