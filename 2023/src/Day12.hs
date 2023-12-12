module Day12 (part1, part2) where

data Spring = NoSpring | Spring | Unknown deriving (Show, Eq, Ord)

mkSpring :: Char -> Spring
mkSpring '.' = NoSpring
mkSpring '#' = Spring
mkSpring '?' = Unknown

line :: Parser ([Spring], [Int])
line = (,) <$> ((mkSpring <$$> many1 (oneOf ".#?")) <* string " ") <*> (number `sepBy` string ",")

validConfigurations :: ([Spring], [Int]) -> [[Spring]]
validConfigurations (ss, ns) = unSeq $ go ss ns False [] False
  where
    log s a = a -- traceShow
    logUn s a = traceShow s a
    logV s a = traceShow s a
    go [] [] _ c _ = logV (reverse c, "valid") $ mkSeq [reverse c]
    go sss@(Spring : ss) nss@(1 : ns) _ c True = logUn (sss, nss, reverse c, "cannot take, just closed") emptySeq
    go sss@(Spring : ss) nss@(1 : ns) _ c False = log (sss, nss, reverse c, "taking spring and closing block") $ go ss ns False (Spring : c) True
    go sss@(Spring : ss) nss@(n : ns) _ c True = logUn (sss, nss, reverse c, "cannot take, just closed") emptySeq
    go sss@(Spring : ss) nss@(n : ns) _ c False = log (sss, nss, reverse c, "taking spring") $ go ss ((n - 1) : ns) True (Spring : c) False
    go sss@(NoSpring : ss) ns False c _ = log (sss, ns, reverse c, "no spring,continuing") $ go ss ns False (NoSpring : c) False
    go sss@(NoSpring : _) ns True c _ = logUn (reverse c, ns, "encountered empty, but in block") $ emptySeq
    go sss@(Unknown : ss) nss@(1 : ns) True c _ = log (sss, nss, reverse c, "inside block,must interpret unknown as spring,closing block") $ go ss ns False (Spring : c) True
    go sss@(Unknown : ss) nss@(n : ns) True c _ = log (sss, nss, reverse c, "inside block,must interpret unknown as spring") $ go ss ((n - 1) : ns) True (Spring : c) False
    go sss@(Unknown : ss) nss False c True = log (sss, nss, reverse c, "unknown must be none, just closed") $ go ss nss False (NoSpring : c) False
    go sss@(Unknown : ss) nss@(1 : ns) False c False = log (sss, nss, reverse c, "branching") $ go ss ns False (Spring : c) True >< go ss nss False (NoSpring : c) False
    go sss@(Unknown : ss) nss@(n : ns) False c False = log (sss, nss, reverse c, "branching") $ go ss ((n - 1) : ns) True (Spring : c) False >< go ss nss False (NoSpring : c) False
    go ss [] inBlock c _
      | all (`elem` [Unknown, NoSpring]) ss = logV (reverse c, "valid") $ mkSeq [(reverse c) ++ replicate (length ss) NoSpring]
      | otherwise = logUn (ss, inBlock, reverse c, "unsatisfiable") emptySeq
    go ss ns inBlock c _ = logUn (ss, ns, inBlock, reverse c, "unsatisfiable") emptySeq

gen :: [Int] -> [Spring] -> [[Spring]] -> [[Spring]]
gen ns [] cs' = filter (valid ns) $ reverse <$> cs'
gen ns (Spring : ss) cs = gen ns ss (filter (potentiallyValid ns) ((Spring :) <$> cs))
gen ns (NoSpring : ss) cs = gen ns ss (filter (potentiallyValid ns) ((NoSpring :) <$> cs))
gen ns (Unknown : ss) cs = gen ns ss (filter (potentiallyValid ns) ([(Spring :), (NoSpring :)] <*> cs))

valid :: [Int] -> [Spring] -> Bool
valid ns ss = ns == (length <$> [c | c <- group ss, uhead c /= NoSpring])

potentiallyValid :: [Int] -> [Spring] -> Bool
potentiallyValid ns cs =
  case length <$> [c | c <- group (reverse cs), uhead c /= NoSpring] of
    [] -> True
    ls -> uinit ls `isPrefixOf` ns

unfold :: ([Spring], [Int]) -> ([Spring], [Int])
unfold (ss, ns) = (intercalate [Unknown] (replicate 5 ss), mconcat (replicate 5 ns))

part1' :: Int
part1' =
  $(input 12)
    & parseLinesWith line
    & fmap (\(ss, ns) -> traceShowF length (gen ns ss [[]]))
    & fmap length
    & sum

part1 :: Int
part1 =
  $(input 12)
    & parseLinesWith line
    & fmap (uncurry choices)
    & sum

pline :: Parser [[Spring]]
pline = do
  (ss, ns) <- (,) <$> (many1 (oneOf ".#?") <* string " ") <*> (number `sepBy` string ",")
  let p = do
        let c n = do
              pre <- NoSpring <$ oneOf ".?"
              a <- (\a -> replicate (length a) NoSpring) <$> try (many (oneOf ".?"))
              b <- (\b -> replicate (length b) Spring) <$> try (count n (oneOf "#?"))
              return (pre : a <> b)
        cs <- forM ns c
        many (oneOf ".?")
        eof
        return $ mconcat cs
  let ss' = '.' : ss
  let v = parseWith p ss'
  return $ traceShowId $ traceShow (ss', ns) $ [v]

patterns :: [Spring] -> [Int] -> [[Spring]]
patterns ss ns = mconcat <$> (uncurry (flip replicate) <$$> addN nToFill minimal')
  where
    -- traceShowId $ mconcat <$> foldl' (\patterns _ -> addOne =<< patterns) [minimal] [1 .. nToFill]

    minimal :: [[Spring]]
    minimal = traceShow (ss, ns) $ traceShowId $ [[]] <> (intercalate [[NoSpring]] [[replicate n Spring] | n <- ns]) <> [[]]
    nToFill = length ss - length (mconcat minimal)
    fill n [slot] = [[]]
    addOne :: [[Spring]] -> [[[Spring]]]
    addOne [slot] = [[(NoSpring : slot)]]
    addOne (slot : springs : rest) = ((NoSpring : slot) : springs : rest) : (([slot, springs] <>) <$> addOne rest)
    minimal' = [(NoSpring, 0)] <> (intercalate [(NoSpring, 1)] [[(Spring, n)] | n <- ns]) <> [(NoSpring, 0)]
    nToFill' = length ss - sum (snd <$> minimal')
    nSlots = (length minimal - 1) `div` 2
    addN 0 springs = [springs]
    addN n [(NoSpring, a)] = [[(NoSpring, a + n)]]
    addN n ((NoSpring, a) : springs : rest) = traceShow (nToFill', nSlots, n, length rest) [((NoSpring, a + b) : springs : rest') | b <- [0 .. n], rest' <- addN b rest]

part2' :: Int
part2' =
  $(input 12)
    & parseLinesWith line
    & fmap unfold
    & fmap (\(ss, ns) -> gen ns ss [[]])
    & fmap length
    & sum

choices :: [Spring] -> [Int] -> Int
choices ss ns = startEvalMemo (goM (NoSpring : ss, ns))
  where
    log s a = a -- traceShow s a
    goM = memo (uncurry go)
    go [] [] = return 1
    go [] ns = log ("M", ns) $ return 0
    go (NoSpring : ss) [] = goM (ss, [])
    go (Spring : _) [] = return 0
    go _ [] = return 1
    -- go (Spring : ss) ns = goS (Spring : ss) ns
    -- go (Unknown: ss) ns = goS (Spring : ss) ns +
    -- go [Spring] [1] = return 1
    -- go (Spring : _) (0 : _) = return 0
    -- go (Spring : Spring : ss) (1 : ns) = return 0
    -- go (Spring : Spring : ss) (n : ns) = goM ((Spring : ss), (n - 1 : ns))
    -- go (Spring : NoSpring : ss) (1 : ns) = goM ((NoSpring : ss), ns)
    -- go (Spring : NoSpring : ss) (n : ns) = return 0
    -- go (Spring : Unknown : ss) (1 : ns) = goM ((NoSpring : ss), ns)
    -- go (Spring : Unknown : ss) (n : ns) = goM ((Spring : ss), (n - 1 : ns))
    go (NoSpring : NoSpring : ss) ns = goM (NoSpring : ss, ns)
    -- go (NoSpring : Spring : ss) (n : ns) = goM (Spring : ss, n : ns)
    go (NoSpring : Spring : ss) (n : ns) = goS (Spring : ss) (n : ns)
    go (NoSpring : Unknown : ss) (n : ns) = (+) <$> goM ((NoSpring : ss), (n : ns)) <*> goS (Spring : ss) (n : ns)
    go (Unknown : ss) ns = (+) <$> goM ((NoSpring : ss), ns) <*> goS (Spring : ss) ns
    go ss ns = log ("M", ss, ns) $ return 0
    goS [] [] = return 1
    -- goS [] [0] = return 1
    goS [] ns = log ("S", ns) $ return 0
    goS ss@(Spring : _) [] = log ("S", ss) $ return 0
    goS [Spring] [1] = return 1
    goS ss@(Spring : _) ns@(0 : _) = log ("S", ss, ns) $ return 0
    goS ss (0 : ns) = goM (ss, ns)
    goS (Spring : NoSpring : ss) (1 : ns) = goM ((NoSpring : ss), ns)
    goS ss@(Spring : Spring : _) ns@(1 : _) = log ("S", ss, ns) $ return 0
    goS (Spring : Spring : ss) (n : ns) = goS (Spring : ss) (n - 1 : ns)
    goS (Spring : Unknown : ss) (1 : ns) = goM ((NoSpring : ss), ns)
    goS (Spring : Unknown : ss) (n : ns) = goS (Spring : Spring : ss) (n : ns)
    goS ss ns = log ("S", ss, ns) $ return 0

matching :: [Spring] -> [Spring] -> Bool
matching [] [] = True
matching (Unknown : ss) (_ : ps) = matching ss ps
matching (s : ss) (p : ps) = s == p && matching ss ps

-- 1577845680756 too hgih
part2 =
  $(exampleInput 12)
    & parseLinesWith line
    -- & parseLinesWith pline
    & fmap unfold
    & fmap (uncurry choices)
    & sum

-- & drop 1
-- & take 1
-- & fmap
--   ( \(ss, ns) ->
--       let ps = nub (patterns ss ns)
--        in traceShow (ss, ns, ps) $
--             length (filter (matching ss) ps)
--   )
-- & sum

-- & fmap length
-- & sum