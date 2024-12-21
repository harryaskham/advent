module Day21 where

{-
+---+---+---+
\| 7 | 8 | 9 |
+---+---+---+
\| 4 | 5 | 6 |
+---+---+---+
\| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+

    +---+---+
    | ^ | A |
+---+---+---+
\| < | v | > |
+---+---+---+

for each pad, get all distances between nodes
state is (loc, loc, loc, numloc, totalspend)
heirusitc is manhattan on numpad →

or propagate back; for each num-to-num, we get many expansions
each one leaves the two robots in different positions
cost of inputting a 0:
all paths of moving r2 from r2loc to 0

-}

import Control.Lens (makeLenses, use, view, (%=), (.=), (?~))
import Data.Map.Strict qualified as M
import Data.Variant
import Data.Variant.Types
import GHC.TypeLits

type Numpad = ".A0123456789"

type Dirpad = ".A^v<>"

type Paths cs = (Cell cs, Cell cs) :|-> [[Cell Dirpad]]

paths :: (SChar '.' :< SymSChars cs, Ord (Cell cs), GridCell (Cell cs)) => cs ▦ ℤ² -> Paths cs
paths g =
  firstPaths ∘ sortOn size <$> foldr1 (M.unionWith (<>)) ([ps c d | c <- coords g, d <- coords g, g |! c ≢ (#"." □), g |! d ≢ (#"." □)])
  where
    firstPaths [] = []
    firstPaths allPaths@(firstPath : _) = takeWhile ((≡ size firstPath) ∘ size) allPaths
    -- removeDots paths =
    --   mkMapWith
    --     (<>)
    --     [ ((from, to), [path])
    --       | ((from, to), paths) <- unMap paths,
    --         let fromP = g |!> from,
    --         let visited path = fst $ foldl' (\(vs, c) dir -> ((g |! c) : vs, move @ℤ (toDir² dir) 1 c)) ([], fromP) path,
    --         path <- paths,
    --         let vs = visited path,
    --         let valid = not (any ((≡ (#"." □))) vs),
    --         traceShow ("from/to/path/visits/valid", toChar from, toChar to, toChar <$> path, toChar <$> vs, valid) $ valid
    --     ]

    init = mkMap [((c, d), []) | c <- cells g, d <- cells g, c ≢ (#"." □), d ≢ (#"." □)]
    ps s e = go s (mk₁ (s, []))
      where
        go s Empty = init
        go s ((c, dirs) :<| q)
          | g |! c ≡ (#"." □) = go s q
          | c ≡ e =
              traceShow ("found e", toChar start, toChar end, dirs) $
                go s q |~ ((start, end), (dirs :))
          | size dirs >= ((+) $@ gridDims g) - 2 = go s q
          | otherwise =
              let ns = [(n, dirs <> [fromDir² dir]) | dir <- enumerate, let n = move @ℤ dir 1 c, n ∈ g, g |! n ≢ (#"." □)]
               in traceShow ("moving from", c, dirs, "neighs", ns, toChar ∘ (g |!) . fst <$> ns) $
                    go s (q >< mk ns)
          where
            start = g |! s
            end = g |! e

numpad :: Numpad ▦ ℤ²
numpad =
  readGrid $
    [txt|789
         456
         123
         .0A|]

numpaths :: Paths Numpad
numpaths = paths numpad

dirpad :: Dirpad ▦ ℤ²
dirpad =
  readGrid $
    [txt|.^A
         <v>|]

fromDir² :: Dir² -> Cell Dirpad
fromDir² = fromChar ∘ toArrow²

toDir² :: Cell Dirpad -> Dir²
toDir² = fromArrow² ∘ toChar

dirpaths :: Paths Dirpad
dirpaths = paths dirpad

data St = St
  { _presses :: [Cell Dirpad],
    _target :: Cell Numpad,
    _entered :: Maybe (Cell Numpad),
    _rsP :: [ℤ²],
    _rnumP :: ℤ²
  }
  deriving (Eq, Ord, Show)

makeLenses ''St

initSt :: ℤ -> St
initSt layers =
  St
    { _presses = [],
      _target = (#A □),
      _entered = Nothing,
      _rsP = replicate (fromIntegral layers) (dirpad |!> (#A □)),
      _rnumP = numpad |!> (#A □)
    }

toButtonsValue :: Text -> ([Cell Numpad], ℤ)
toButtonsValue code =
  let cs = unpack code
   in ([fromChar c | c <- cs], take 3 cs |- number)

ixℤ i = ix (fromIntegral i)

h :: St -> ℤ
h st = size $ st ^. presses

pause = False

mpauseId = if pause then pauseId else id

h' :: St -> ℤ
h' st
  | any (≡ True) [dirpad |? rP ∈ [Nothing, Just (#"." □)] | rP <- st ^. rsP] = ꝏ
  | numpad |? (st ^. rnumP) ∈ [Nothing, Just (#"." □)] = ꝏ
  | otherwise =
      traceShow ("h' start", st) $
        let rnum = numpad |! (st ^. rnumP)
         in case numpaths |! (rnum, st ^. target) of
              -- i.e. how many presses already + how many to have the robots input us to target plus an A press
              [] -> traceShow ("no paths", toChar rnum, toChar (st ^. target)) $ ꝏ
              paths ->
                mpauseId ∘ traceShow ("h' rnum", paths, dbg st) $
                  size (st ^. presses) + run do
                    sizes <- forM paths $ \path -> do
                      cost <- robocost .$. (0, (path <> [(#A □)]))
                      traceShow ("h' path", path, cost) $ return cost
                    traceShow ("minimum top", sizes) $
                      if null sizes then traceShow ("no path top", paths) (return ꝏ) else return (minimum sizes)
  where
    dbg st = (st ^. target, st ^. rnumP, st ^. rsP)
    robocost (layer, path)
      | layer ≡ (size (st ^. rsP)) =
          traceShow ("robocost final layer", size path, path, dbg st) $
            return (size path)
      | otherwise = do
          let rP = dirpad |! ((st ^. rsP) !! layer)
          -- the inputs that make this robot input the path
          -- i.e. from current pos to first pos, an A press, repeatas to take including the button presses between
          let segments = zip (rP : path) path
          mpauseId ∘ traceShow ("robocost layer", layer, path, segments, dbg st) $ do
            segmentCosts <- forM segments $ \(from, to) -> do
              let segmentPaths =
                    mpauseId ∘ traceShow ("robocost segment; layer", layer, (from, to), dbg st) $
                      (dirpaths |! (from, to))
              let segmentPathCost segmentPath =
                    mpauseId ∘ traceShow ("robocost segment; layer", layer, (from, to), segmentPaths, dbg st) $
                      robocost .$. (layer + 1, segmentPath <> [(#A □)])
              sizes <- sequence $ segmentPathCost <$> segmentPaths
              mpauseId ∘ traceShow ("robocost segment; layer", layer, (from, to), sizes, dbg st) $
                traceShow ("minimum layer", layer, sizes) $
                  if null sizes then return ꝏ else return (minimum sizes)
            return $ sum segmentCosts

push :: St -> St
push st = go ø (mkQ₁ h st)
  where
    -- in traceShow ("robocost", layer, toChar <$> path) $
    --     minimum $
    --       (\path -> robocost (layer + 1) (path <> [(#A □)])) <$> paths
    key st = (st ^. rsP, st ^. rnumP)
    go seen ((cost, st) :<! q)
      | (st ^. entered) ≡ Just (st ^. target) = st
      | key st ∈ seen = go seen q
      | any (≡ True) [dirpad |? rP ∈ [Nothing, Just (#"." □)] | rP <- st ^. rsP] = go seen' q
      | numpad |? (st ^. rnumP) ∈ [Nothing, Just (#"." □)] = go seen' q
      | incorrectEntry st = go seen' q
      | otherwise =
          traceShow (cost, size seen, st ^. target, key st) $
            go seen' $
              qAppend h [humanPress st (fromChar pressed) & presses %~ (fromChar pressed :) | pressed <- "<>^vA"] q
      where
        seen' = key st |-> seen
    humanPress :: St -> Cell Dirpad -> St
    humanPress st pressed
      | pressed ≡ (#A □) = rPress 0 st
      | otherwise = st & rsP ∘ ixℤ 0 %~ move @ℤ (toDir² pressed) 1
    rPress :: ℤ -> St -> St
    rPress layer st =
      let rP = (st ^. rsP) !! layer
          pressed = dirpad |! rP
       in if pressed ≡ (#A □)
            then
              if layer ≡ (size (st ^. rsP)) - 1
                then rnumPress st
                else rPress (layer + 1) st
            else
              if layer ≡ (size (st ^. rsP)) - 1
                then st & rnumP %~ move @ℤ (toDir² pressed) 1
                else st & rsP ∘ ixℤ (layer + 1) %~ move @ℤ (toDir² pressed) 1
    rnumPress :: St -> St
    rnumPress st =
      let pressed = numpad |! (st ^. rnumP)
       in st & entered ?~ pressed
    incorrectEntry :: St -> 𝔹
    incorrectEntry st = case st ^. entered of
      Nothing -> False
      Just entered -> entered ≢ (st ^. target)

onecode :: Text -> ℤ -> ℤ
onecode s = complexity [s]

complexity :: [Text] -> ℤ -> ℤ
complexity buttons layers =
  buttons
    & fmap toButtonsValue
    & fmap
      ( \(buttons, value) ->
          traceShow (buttons, value) $
            let (ps, _) =
                  foldl'
                    -- (\(ps, rnumP') button -> let st = push (initSt layers & rnumP .~ rnumP' & target .~ button) in (ps + size (st ^. presses), st ^. rnumP))
                    ( \(ps, rnumP') button ->
                        traceShow ("in fold", ps, rnumP', button) $
                          ( ps
                              + h'
                                ( St
                                    { _presses = [],
                                      _entered = Nothing,
                                      _target = button,
                                      _rnumP = rnumP',
                                      _rsP = replicate (fromIntegral layers) (dirpad |!> (#A □))
                                    }
                                ),
                            numpad |!> button
                          )
                    )
                    (0, numpad |!> (#A □))
                    buttons
             in ps ⋅ value
      )
    & sum

part1 :: ℤ
part1 = complexity (lines $(aoc 21)) 2

part2 :: ℤ
part2 = complexity (lines $(aoc 21)) 25
