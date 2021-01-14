module TwentyEighteen.Day13 where

import Coord (Coord2, Dir2 (..), move, turnCCW, turnCW)
import Data.List (foldl', sortOn)
import Data.List.Extra (groupOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (swap)
import Grid (Grid, toGrid)
import Util (countMap, input)

newtype CartId = CartId Int deriving (Show, Ord, Eq)

data Cart = Cart CartId Dir2 Coord2 Turn deriving (Show)

data Turn = TurnLeft | Straight | TurnRight deriving (Show)

rotateTurn :: Turn -> Turn
rotateTurn TurnLeft = Straight
rotateTurn Straight = TurnRight
rotateTurn TurnRight = TurnLeft

replaceCart :: Char -> Char
replaceCart 'v' = '|'
replaceCart '^' = '|'
replaceCart '>' = '-'
replaceCart '<' = '-'
replaceCart x = x

toCart :: (Int, (Coord2, Char)) -> Maybe Cart
toCart (cid, (pos, 'v')) = Just $ Cart (CartId cid) DirDown pos TurnLeft
toCart (cid, (pos, '^')) = Just $ Cart (CartId cid) DirUp pos TurnLeft
toCart (cid, (pos, '>')) = Just $ Cart (CartId cid) DirRight pos TurnLeft
toCart (cid, (pos, '<')) = Just $ Cart (CartId cid) DirLeft pos TurnLeft
toCart _ = Nothing

cartPos :: Cart -> Coord2
cartPos (Cart _ _ pos _) = pos

cartId :: Cart -> CartId
cartId (Cart cid _ _ _) = cid

stepCart :: Grid Char -> Cart -> Cart
stepCart grid (Cart cid dir pos turn) = Cart cid nextDir nextPos nextTurn
  where
    nextPos = move dir 1 pos
    track = grid M.! nextPos
    nextDir = case track of
      '|' -> dir
      '-' -> dir
      '+' -> case turn of
        Straight -> dir
        TurnLeft -> turnCCW dir
        TurnRight -> turnCW dir
      '/' -> case dir of
        DirUp -> DirRight
        DirDown -> DirLeft
        DirLeft -> DirDown
        DirRight -> DirUp
      '\\' -> case dir of
        DirUp -> DirLeft
        DirDown -> DirRight
        DirLeft -> DirUp
        DirRight -> DirDown
    nextTurn = case track of
      '+' -> rotateTurn turn
      _ -> turn

tick :: Grid Char -> [Cart] -> [Cart]
tick grid carts = stepCart grid <$> sortOn cartPos carts

tickUntilCrash :: Grid Char -> [Cart] -> [Coord2]
tickUntilCrash grid carts
  | isCrash = M.keys $ M.filter (> 1) posCounts
  | otherwise = tickUntilCrash grid (tick grid carts)
  where
    posCounts = countMap (cartPos <$> carts)
    isCrash = M.size posCounts < length carts

readGrid :: IO (Grid Char, [Cart])
readGrid = do
  grid' <- toGrid id . lines <$> input 2018 13
  let carts = mapMaybe toCart (zip [0 ..] (M.toList grid'))
      grid = replaceCart <$> grid'
  return (grid, carts)

part1 :: IO [Coord2]
part1 = uncurry tickUntilCrash <$> readGrid

tickWithCrashes :: Grid Char -> [Cart] -> [Cart]
tickWithCrashes grid carts =
  go (M.fromList [(cartId c, c) | c <- carts]) (cartId <$> sortedCarts)
  where
    sortedCarts = sortOn (swap . cartPos) carts
    go :: Map CartId Cart -> [CartId] -> [Cart]
    go carts [] = M.elems carts
    go carts (cId : cIds) = go nextCarts cIds
      where
        steppedCarts = M.adjust (stepCart grid) cId carts
        toDelete = crashedIds steppedCarts
        nextCarts = foldl' (flip M.delete) steppedCarts toDelete

crashedIds :: Map CartId Cart -> [CartId]
crashedIds carts =
  fmap fst
    . concat
    . filter ((> 1) . length)
    . groupOn snd
    . M.toList
    $ cartPos <$> carts

tickUntilLastCart :: Grid Char -> [Cart] -> Cart
tickUntilLastCart _ [cart] = cart
tickUntilLastCart grid carts =
  tickUntilLastCart grid (tickWithCrashes grid carts)

part2 :: IO Coord2
part2 = cartPos . uncurry tickUntilLastCart <$> readGrid
