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

import Control.Lens (makeLenses, use, view, (%=), (.=))
import Data.Map.Strict qualified as M
import Data.Variant
import Data.Variant.Types
import GHC.TypeLits

type Numpad = ".A0123456789"

type Dirpad = ".A^v<>"

type Paths cs = (Cell cs, Cell cs) :|-> [[Cell Dirpad]]

paths :: (SChar '.' :< SymSChars cs, Ord (Cell cs), GridCell (Cell cs)) => cs ▦ ℤ² -> Paths cs
paths g = sortOn size <$> foldl1 (M.unionWith (<>)) (ps <$> cells g <*> pure g)
  where
    init = mkMap [((c, d), []) | c <- cells g, d <- cells g]
    ps start g = go (mk₁ (s, [])) ø
      where
        s = g |!> start
        go Empty _ = init
        go ((c, dirs) :<| q) seen
          | c ∈ seen = go q seen
          | g |! c ≡ (#"." □) = go q seen
          -- \| size dirs > ((*) $@ gridDims g) = go q seen
          | otherwise =
              let ns = [(n, (fromDir² <$> goingTo c n) <> dirs) | n <- neighs @4 c g]
                  seen' = c |-> seen
                  rest = go (q >< mk ns) seen'
               in rest |~ ((g |! s, g |! c), (dirs :))

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

dirpaths :: Paths Dirpad
dirpaths = paths dirpad

data St = St
  { _presses :: [Cell Dirpad],
    _ra :: Cell Dirpad,
    _rb :: Cell Dirpad,
    _rnum :: Cell Numpad
  }
  deriving (Eq, Ord, Show)

makeLenses ''St

type M = StateT St IO

runSt :: M a -> IO a
runSt = flip evalStateT $ St [] (#A □) (#A □) (#A □)

branch :: M a -> M a
branch action = do
  st <- get
  a <- action
  put st
  return a

codeinput :: [Cell Numpad] -> M ()
codeinput [] = return ()
codeinput (c : cs) = do
  numinput c
  codeinput cs

numinput :: Cell Numpad -> M ()
numinput c = do
  rnum' <- use rnum
  let ps = numpaths |! (rnum', c)
  let p = ps !! 0
  putTextLn $ "RNum moving " <> (as $ toChar rnum') <> " to " <> (as $ toChar c) <> " via path: " <> (pack $ toChar <$> p)
  forM p $ \button -> do
    rbinput button
  rbinput (#A □)
  putTextLn $ "RNum presses: " <> (as $ toChar c)
  rnum .= c

rbinput :: Cell Dirpad -> M ()
rbinput c = do
  rb' <- use rb
  let ps = dirpaths |! (rb', c)
  let p = ps !! 0
  putTextLn $ "RB moving " <> (as $ toChar rb') <> " to " <> (as $ toChar c) <> " via path: " <> (pack $ toChar <$> p)
  forM p $ \button -> do
    rainput button
  rainput (#A □)
  putTextLn $ "RB presses: " <> (as $ toChar c)
  rb .= c

rainput :: Cell Dirpad -> M ()
rainput c = do
  ra' <- use ra
  let ps = dirpaths |! (ra', c)
  let p = ps !! 0
  putTextLn $ "RA moving " <> (as $ toChar ra') <> " to " <> (as $ toChar c) <> " via path: " <> (pack $ toChar <$> p)
  forM p $ \button -> do
    humaninput button
  humaninput (#A □)
  putTextLn $ "RA presses: " <> (as $ toChar c)
  ra .= c

humaninput :: Cell Dirpad -> M ()
humaninput c = do
  putTextLn $ "Human presses: " <> (as $ toChar c)
  presses %= (c :)

toButtonsValue :: Text -> ([Cell Numpad], ℤ)
toButtonsValue code =
  let cs = unpack code
   in ([fromChar c | c <- cs], take 3 cs |- number)

part1 :: ℤ
part1 = unsafePerformIO do
  let buttonsValue = toButtonsValue <$> lines $(aocx 21)
  complexities <- forM buttonsValue $ \(buttons, value) -> do
    putTextLn $ "Code: " <> tshow (buttons, value)
    runSt do
      codeinput buttons
      prs <- use presses
      putTextLn $ "Presses: " <> tshow (toChar <$> reverse prs)
      putTextLn $ "Num presses: " <> tshow (size prs)
      return $ size prs ⋅ value
  return $ sum complexities

part2 :: Text
part2 = "Part 2"
