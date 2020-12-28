module TwentySeventeen.Day20 where

import Data.List (minimumBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
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
import Util (readWithParser)

type V3 = (Int, Int, Int)

type Particle = (V3, V3, V3)

inputPath :: String
inputPath = "input/2017/20.txt"

particles :: GenParser Char () [Particle]
particles = do
  ps <- many particle
  eof
  return ps
  where
    particle = do
      xs <- vec3 `sepBy` string ", "
      char '\n'
      return $ let [p, v, a] = xs in (p, v, a)
    vec3 = do
      oneOf "pva"
      char '='
      xs <- between (char '<') (char '>') (number `sepBy` char ',')
      return $ let [x1, x2, x3] = xs in (read x1, read x2, read x3)
    number = many1 (oneOf "-0123456789")

part1 :: IO Int
part1 = do
  ps <- readWithParser particles <$> readFile inputPath
  return . fst $
    minimumBy
      (comparing (\(_, (_, _, (ax, ay, az))) -> abs ax + abs ay + abs az))
      (zip [0 ..] ps)

stepParticle :: Particle -> Particle
stepParticle ((x, y, z), (vx, vy, vz), (ax, ay, az)) =
  let vx' = vx + ax
      vy' = vy + ay
      vz' = vz + az
      x' = x + vx'
      y' = y + vy'
      z' = z + vz'
   in ((x', y', z'), (vx', vy', vz'), (ax, ay, az))

removeOverlaps :: [Particle] -> [Particle]
removeOverlaps ps = concat $ M.elems . M.filter ((== 1) . length) $ posToPs
  where
    posToPs = M.fromListWith (++) [(pos, [p]) | p@(pos, _, _) <- ps]

stepParticles :: Int -> [Particle] -> [Particle]
stepParticles 0 ps = ps
stepParticles n ps = stepParticles (n - 1) ps'
  where
    ps' = removeOverlaps $ stepParticle <$> ps

part2 :: IO Int
part2 = do
  ps <- readWithParser particles <$> readFile inputPath
  return $ length (stepParticles 1000 ps)
