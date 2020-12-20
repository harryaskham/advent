module TwentyTwenty.Day20 where

import Data.List (intersect, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    count,
    digit,
    eof,
    many,
    many1,
    oneOf,
    sepBy,
    string,
  )
import Util (readWithParser)

inputPath :: String
inputPath = "input/2020/20.txt"

data Tile = Tile Int (M.Map (Int, Int) Char) deriving (Eq)

tile :: GenParser Char st Tile
tile = do
  string "Tile "
  tileId <- many1 digit
  string ":\n"
  rows <- many $ do
    r <- count 10 (oneOf "#.")
    char '\n'
    return r
  return $
    Tile
      (read tileId)
      ( M.fromList
          [ ((x, y), c)
            | (y, row) <- zip [0 ..] rows,
              (x, c) <- zip [0 ..] row
          ]
      )

tiles :: GenParser Char st [Tile]
tiles = do
  tiles <- tile `sepBy` char '\n'
  eof
  return tiles

maxXY :: M.Map (Int, Int) a -> (Int, Int)
maxXY m = (maximum $ fst <$> M.keys m, maximum $ snd <$> M.keys m)

modifyCoords :: ((Int, Int) -> (Int, Int)) -> Tile -> Tile
modifyCoords f (Tile tId tiles) =
  Tile tId (M.mapKeys (fromOrigin . f . toOrigin) tiles)
  where
    (maxX, maxY) = maxXY tiles
    xO = (maxX + 1) `div` 2
    yO = (maxY + 1) `div` 2
    toOrigin (x, y) = (x - xO, y - yO)
    fromOrigin (x, y) = (x + xO, y + yO)

variants :: Tile -> [Tile]
variants t = nub $ modifyCoords <$> mods <*> pure t
  where
    flipH (x, y) = (negate x - 1, y)
    flipV (x, y) = (x, negate y - 1)
    rot90 (x, y) = (y, negate x - 1)
    rot180 = rot90 . rot90
    rot270 = rot90 . rot90 . rot90
    mods = (.) <$> [id, flipH, flipV] <*> [id, rot90, rot180, rot270]

mkVariantMap :: [Tile] -> M.Map Int [Tile]
mkVariantMap ts =
  M.fromList $
    (\t@(Tile tId _) -> (tId, variants t)) <$> ts

edgeString :: [(Int, Int)] -> Tile -> String
edgeString coords (Tile _ tiles) = (tiles M.!) <$> coords

leftEdge :: Tile -> String
leftEdge = edgeString [(0, y) | y <- [0 .. 9]]

rightEdge :: Tile -> String
rightEdge = edgeString [(9, y) | y <- [0 .. 9]]

topEdge :: Tile -> String
topEdge = edgeString [(x, 0) | x <- [0 .. 9]]

bottomEdge :: Tile -> String
bottomEdge = edgeString [(x, 9) | x <- [0 .. 9]]

mkEdgeMap :: (Tile -> String) -> [Tile] -> M.Map String [Tile]
mkEdgeMap getEdge ts =
  M.fromListWith (++) (zip (getEdge <$> ts) (pure <$> ts))

type Jigsaw = M.Map (Int, Int) Tile

solvedJigsaws ::
  Int ->
  M.Map Int [Tile] ->
  M.Map String [Tile] ->
  M.Map String [Tile] ->
  (Int, Int) ->
  S.Set Int ->
  Jigsaw ->
  [Jigsaw]
solvedJigsaws width variantMap leftEdgeMap topEdgeMap (jx, jy) tileIdsPlaced jigsaw =
  if jy == width
    then [jigsaw]
    else case validVariants of
      [] -> []
      ts -> concatMap choosingVariant ts
  where
    unseen (Tile tId _) = not $ tId `S.member` tileIdsPlaced
    rightE = rightEdge <$> M.lookup (jx - 1, jy) jigsaw
    bottomE = bottomEdge <$> M.lookup (jx, jy - 1) jigsaw
    leftMatches =
      case rightE of
        Just e -> filter unseen . fromMaybe [] $ M.lookup e leftEdgeMap
        Nothing -> []
    topMatches =
      case bottomE of
        Just e -> filter unseen . fromMaybe [] $ M.lookup e topEdgeMap
        Nothing -> []
    validVariants =
      case (jx, jy) of
        (0, 0) -> concatMap snd (M.toList variantMap)
        (_, 0) -> leftMatches
        (0, _) -> topMatches
        (_, _) -> leftMatches `intersect` topMatches
    nextCoord =
      if jx == width - 1
        then (0, jy + 1)
        else (jx + 1, jy)
    choosingVariant t@(Tile tId _) =
      solvedJigsaws
        width
        variantMap
        leftEdgeMap
        topEdgeMap
        nextCoord
        (S.insert tId tileIdsPlaced)
        (M.insert (jx, jy) t jigsaw)

solveJigsaw :: IO (Jigsaw, Int)
solveJigsaw = do
  ts <- readWithParser tiles <$> readFile inputPath
  let width = round $ sqrt . fromIntegral $ length ts
      allVariants = concatMap variants ts
      variantMap = mkVariantMap ts
      leftEdgeMap = mkEdgeMap leftEdge allVariants
      topEdgeMap = mkEdgeMap topEdge allVariants
      solutions =
        solvedJigsaws
          width
          variantMap
          leftEdgeMap
          topEdgeMap
          (0, 0)
          S.empty
          M.empty
  return (head solutions, width)

getId :: Tile -> Int
getId (Tile tId _) = tId

getTiles :: Tile -> M.Map (Int, Int) Char
getTiles (Tile _ tiles) = tiles

part1 :: IO Int
part1 = do
  (jigsaw, width) <- solveJigsaw
  let corners =
        (jigsaw M.!)
          <$> [ (0, 0),
                (0, width - 1),
                (width - 1, 0),
                (width - 1, width - 1)
              ]
  return $ product $ getId <$> corners

getJT :: Jigsaw -> Int -> Int -> Int -> Int -> Char
getJT jigsaw jx jy tx ty =
  getTiles (jigsaw M.! (jx, jy)) M.! (tx, ty)

concatJigsaw :: Jigsaw -> Int -> Tile
concatJigsaw jigsaw width =
  Tile
    9999
    ( M.fromList
        [ ( (jx * 8 + (tx - 1), jy * 8 + (ty - 1)),
            getJT jigsaw jx jy tx ty
          )
          | jx <- [0 .. width - 1],
            jy <- [0 .. width - 1],
            tx <- [1 .. 8],
            ty <- [1 .. 8]
        ]
    )

monsterCoords :: [(Int, Int)]
monsterCoords =
  [ (x, y)
    | (y, row) <- zip [0 ..] ls,
      (x, c) <- zip [0 ..] row,
      c == '#'
  ]
  where
    ls =
      [ "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
      ]

monsterCoordsAtOffset :: M.Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
monsterCoordsAtOffset tiles (xO, yO) =
  if monsterCells == replicate (length monsterCoords) '#'
    then mCoords
    else []
  where
    mCoords = [(x + xO, y + yO) | (x, y) <- monsterCoords]
    monsterCells = catMaybes $ M.lookup <$> mCoords <*> pure tiles

allMonsterCoords :: M.Map (Int, Int) Char -> S.Set (Int, Int)
allMonsterCoords tiles =
  S.fromList (concatMap (monsterCoordsAtOffset tiles) offsets)
  where
    (maxX, maxY) = maxXY tiles
    offsets = [(xO, yO) | xO <- [0 .. maxX], yO <- [0 .. maxY]]

nonMonsterCoords :: M.Map (Int, Int) Char -> [(Int, Int)]
nonMonsterCoords tiles =
  if null mCoords
    then []
    else
      [ (x, y)
        | x <- [0 .. maxX],
          y <- [0 .. maxY],
          tiles M.! (x, y) == '#',
          not ((x, y) `S.member` mCoords)
      ]
  where
    (maxX, maxY) = maxXY tiles
    mCoords = allMonsterCoords tiles

part2 :: IO Int
part2 = do
  (jigsaw, width) <- solveJigsaw
  let tile = concatJigsaw jigsaw width
      images = getTiles <$> variants tile
  return
    . length
    . head
    . filter (not . null)
    $ nonMonsterCoords <$> images
