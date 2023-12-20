module Day20 (part1, part2) where

import Data.Tuple.Extra (thd3)

mkInitialState :: Map String [String] -> (Int, Int, [Map String (Map String Bool, [Bool])], Bool)
mkInitialState g =
  let gR = mkMapWith (<>) [(v, [k]) | (k, vs) <- unMap g, v <- vs]
      initialInputs "broadcaster" = mkMap []
      initialInputs ('%' : _) = mkMap []
      initialInputs ('&' : name) = mkMap [(inputName, False) | inputName <- gR |! name]
      s = mkMap [(name, (initialInputs name, [False])) | name <- keys g]
   in (0, 0, [s], False)

pushButton :: Int -> Map String [String] -> (Int, Int, [Map String (Map String Bool, [Bool])], Bool)
pushButton n g =
  let (low, high, ss, satisfied) = iterate (onePass g) (mkInitialState g) !! n in (n + low, high, reverse ss, satisfied)

onePass :: Map String [String] -> (Int, Int, [Map String (Map String Bool, [Bool])], Bool) -> (Int, Int, [Map String (Map String Bool, [Bool])], Bool)
onePass g st = go st (mkSeq [("button", "broadcaster", False)])
  where
    targets :: String -> Maybe ([String], String)
    targets name =
      -- traceShow (name) $
      case mapMaybe (\n -> (,n) <$> (g |? n)) ([id, ('%' :), ('&' :)] <*> pure name) of
        [] -> Nothing
        (ts, fullName) : _ -> Just (ts, fullName)
    go st Empty = st
    go (low, high, ss@(s : _), satisfied) (event@(origin, current, incomingSignal) :<| stack) =
      -- traceShow (st, event) $
      -- traceShow (event) $
      let Just (ts, fullName) = targets current
          (inputs, signals@(signal:_)) = s |! fullName
          inputs' = inputs |. (origin, incomingSignal)
          signal' = case fullName of
            "broadcaster" -> Just False
            ('%' : _) -> if incomingSignal then Nothing else Just (not signal)
            ('&' : _) -> if all snd (unMap inputs') then Just False else Just True
          s' = s |~ (fullName, first (const inputs'))
          st'@(low', high', s'', satisfied') =
            let satisfied' = satisfied || (("rx" :: String) ∈ (ts :: [String]) && signal' == Just False)
             in case signal' of
                  Nothing -> (low, high, s' : ss, satisfied')
                  Just False -> (low + length ts, high, (s' |~ (fullName, second ( False:))) : ss, satisfied')
                  Just True -> (low, high + length ts, (s' |~ (fullName, second (True:))) : ss, satisfied')
       in -- traceShow (st', origin, fullName) $
          -- traceShow (fullName, ts, st') $
          -- if fullName /= "broadcaster" && origin /= "broadcaster" && not (any (\((c : _), (_, signal)) -> c == '%' && signal) (unMap s''))
          --   then traceShow "cycle over" $ st'
          --   else go st' $ case signal' of
          --     Nothing -> stack
          --     Just signalToSend -> stack >< mkSeq [(fullName, t, signalToSend) | t <- ts]
          go st' $ case signal' of
            Nothing -> stack
            Just signalToSend -> stack >< mkSeq [(fullName, t, signalToSend) | t <- ts, isJust (targets t)]

-- pushButton' :: Int -> Map String [String] -> (Int, Int, [Map String (Map String Bool, Bool)], Bool, Buul, Map String [Buul])
-- pushButton' n g =
  -- let (low, high, [s], satisfied) = mkInitialState g
   -- in iterate (onePass' g) (low, high, [s |. ("rx", (mkMap [], False))], satisfied, Val 1, ([] <$ g) |. ("broadcaster", [Val 1])) !! n
--
-- onePass' :: Map String [String] -> (Int, Int, [Map String (Map String Bool, Bool)], Bool, Buul, Map String [Buul]) -> (Int, Int, [Map String (Map String Bool, Bool)], Bool, Buul, Map String [Buul])
-- onePass' g st = go st (mkSeq [("button", "broadcaster", False)])
  -- where
    -- gR = mkMapWith (<>) [(v, [k]) | (k, vs) <- unMap g, v <- vs]
    -- targets name =
      -- case mapMaybe (\n -> (,n) <$> (g |? n)) ([id, ('%' :), ('&' :)] <*> pure name) of
        -- [] -> ([], "rx")
        -- (ts, fullName) : _ -> (ts, fullName)
    -- go :: (Int, Int, [Map String (Map String Bool, Bool)], Bool, Buul, Map String [Buul]) -> Seq (String, String, Bool) -> (Int, Int, [Map String (Map String Bool, Bool)], Bool, Buul, Map String [Buul])
    -- go st Empty = st
    -- go (low, high, ss@(s : _), satisfied, conditions, allConditions) (event@(origin, current, incomingSignal) :<| stack) =
      -- -- traceShow event $
      -- let (ts, fullName) = targets current
          -- (inputs, signal) = case s |? fullName of
            -- Nothing -> (mkMap [], False)
            -- Just insig -> insig
          -- inputs' = inputs |. (origin, incomingSignal)
          -- signal' = case fullName of
            -- "broadcaster" -> Just False
            -- "rx" -> Nothing
            -- ('%' : _) -> if incomingSignal then Nothing else Just (not signal)
            -- ('&' : _) -> if all snd (unMap inputs') then Just False else Just True
          -- s' = s |~ (fullName, first (const inputs'))
          -- st' =
            -- let satisfied' = satisfied || (("rx" :: String) ∈ (ts :: [String]) && signal' == Just False)
                -- conditions' = case fullName of
                  -- "broadcaster" -> Val 1
                  -- "rx" -> conditions
                  -- '%' : _ -> Dubble conditions
                  -- '&' : name -> case gR |? name of
                    -- Just fns -> (And (LazyVal <$> fns))
                    -- Nothing -> Val 0
                -- allConditions' = allConditions |~ (fullName, (conditions :))
             -- in case signal' of
                  -- Nothing -> (low, high, [s'], satisfied', conditions', allConditions')
                  -- Just False -> (low + length ts, high, [(s' |~ (fullName, second (const False)))], satisfied', conditions', allConditions')
                  -- Just True -> (low, high + length ts, [(s' |~ (fullName, second (const True)))], satisfied', conditions', allConditions')
       -- in -- traceShow (st', origin, fullName) $
          -- -- traceShow (fullName, ts, st') $
          -- -- if fullName /= "broadcaster" && origin /= "broadcaster" && not (any (\((c : _), (_, signal)) -> c == '%' && signal) (unMap s''))
          -- --   then traceShow "cycle over" $ st'
          -- --   else go st' $ case signal' of
          -- --     Nothing -> stack
          -- --     Just signalToSend -> stack >< mkSeq [(fullName, t, signalToSend) | t <- ts]
          -- go st' $ case signal' of
            -- Nothing -> stack
            -- Just signalToSend -> stack >< mkSeq [(fullName, t, signalToSend) | t <- ts]

-- pushUntil :: Map String [String] -> Int
-- pushUntil g = go 0 (mkInitialState g)
--   where
--     go n (_, _, _, True) = n
--     go n st = traceShow n $ go (n + 1) (onePass' g st)

nameToHistory :: Int -> Map String [String] -> String -> [Bool]
nameToHistory n g name = let (_, _, history, _) = pushButton n g in (\h -> reverse $ snd (h |! name)) =<< history

requirements :: Map String [String] -> (String, Bool) -> [(String, Bool)]
requirements g a = go a
  where
    targets :: String -> Maybe ([String], String)
    targets name =
      case mapMaybe (\n -> (,n) <$> (g |? n)) ([id, ('%' :), ('&' :)] <*> pure name) of
        [] -> Nothing
        (ts, fullName) : _ -> Just (ts, fullName)
    gR = mkMapWith (<>) [(v, [k]) | (k, vs) <- unMap g, v <- vs]
    go ("broadcaster", False) = []
    go (name, signal) =
      -- traceShow (name, gR) $
      case targets name of
        Nothing -> mconcat [go (precursor, signal) | precursor <- gR |! dropWhile (∈ ("%&" :: String)) name]
        Just (_, fullName@('&' : _)) ->
          if not signal
            then mconcat [go (drop 1 precursor, not signal) | precursor <- gR |! dropWhile (∈ ("%&" :: String)) name]
            else []
        --Just (_, fullName@('%' : _)) -> [(fullName, signal)]
        Just (_, fullName@('%' : _)) -> mconcat [go (drop 1 precursor, not signal) | precursor <- gR |! dropWhile (∈ ("%&" :: String)) name]

part1 :: Int
part1 =
  $(input 20)
    |- ( mkMap
           <$> ( many1
                   ( (,)
                       <$> (many1 (oneOf "%&" <|> alphaNum) <* string " -> ")
                       <*> (many1 alphaNum `sepBy1` string ", " <* eol)
                   )
                   <* eof
               )
       )
    & pushButton 1000
    & (\(low, high, _, _) -> (low, high))
    & traceShowId
    & uncurry (*)

findCycle :: (Ord a) => [a] -> [a]
findCycle xs = uhead . uhead $ [cs | n <- [1 .. length xs], let cs = chunksOf n xs, length (nub cs) == 1]

part2' :: Int
part2' =
  $(input 20)
    |- ( mkMap
           <$> ( many1
                   ( (,)
                       <$> (many1 (oneOf "%&" <|> alphaNum) <* string " -> ")
                       <*> (many1 alphaNum `sepBy1` string ", " <* eol)
                   )
                   <* eof
               )
       )
    -- & pushUntil
    -- & ( \g ->
    --       ( first
    --           ( \name ->
    --               let h = nameToHistory 10000 g name
    --                   pairs = zip [0 ..] $ zip h (drop 1 h)
    --                   changes = [p | p@(i, (a, b)) <- pairs, a /= b]
    --                   getDiffs ((i, _) : (j, _) : rest) = (i, j, j - i) : getDiffs rest
    --                   getDiffs _ = []
    --                   diffs = getDiffs changes
    --               in (name, diffs)
    --           )
    --       )
    --         <$> requirements g ("rx", False)
    --   )
    -- & traceShowId
    -- & fmap (first (findCycle . fmap thd3 . snd))
    -- & traceShowId
    -- & length
    & ( \g ->
          ( first
              ( \name ->
                  let h = nameToHistory 10000 g name
                      pairs = zip [0 ..] $ zip h (drop 1 h)
                      changes = [p | p@(i, (a, b)) <- pairs, a /= b]
                      getDiffs ((i, _) : (j, _) : rest) = (i, j, j - i) : getDiffs rest
                      getDiffs _ = []
                      diffs = getDiffs changes
                   in (name, diffs)
              )
          )
            <$> requirements g ("rx", False)
      )
    -- was about to speed up button pushing and then meybe merge cycles (but this cant exceed the cap)
    & traceShowId
    & fmap (first (findCycle . fmap thd3 . snd))
    & traceShowId
    & length

part2 =
  $(input 20)
    |- ( mkMap
           <$> ( many1
                   ( (,)
                       <$> (many1 (oneOf "%&" <|> alphaNum) <* string " -> ")
                       <*> (many1 alphaNum `sepBy1` string ", " <* eol)
                   )
                   <* eof
               )
       )
      & ( \g ->
            first
              ( \name ->
                  let h = nameToHistory 10000 g name
                      pairs = zip [0 ..] $ zip h (drop 1 h)
                      changes = [i | p@(i, (a, b)) <- pairs, a /= b]
                   in -- getDiffs ((i, _) : (j, _) : rest) = (i, j, j - i) : getDiffs rest
                      -- getDiffs _ = []
                      -- diffs = getDiffs changes
                      uhead changes
              )
              <$> (traceShowId $ requirements g ("rx", False))
        )
      & traceShowId
      & fmap fst
      & foldl1 lcm

data Buul = And [Buul] | Or [Buul] | Val Int | LazyVal String | Dubble Buul deriving (Eq, Ord, Show)

eval :: Map String Buul -> Buul -> Buul
eval g (And [b]) = eval g b
eval g (Or [b]) = eval g b
eval g (And bs) = And (eval g <$> bs)
eval g (Or bs) = Or (eval g <$> bs)
eval g (Val i) = Val i
eval g (LazyVal s) = eval g (g |! s)
eval g (Dubble b) = dubble g (eval g b)

dubble :: Map String Buul -> Buul -> Buul
dubble g (And bs) = And (dubble g <$> bs)
dubble g (Or bs) = Or (dubble g <$> bs)
dubble g (Val i) = Val (2 * i)
dubble g (LazyVal s) = Dubble (LazyVal s)
dubble g (Dubble b) = dubble g b

-- lowWhen :: Map String [String] -> Map String Buul
-- lowWhen g = go (mkSeq [("broadcaster", False)]) (Val 0) ([] <$ g)
--   where
--     gR = mkMapWith (<>) [(v, [k]) | (k, vs) <- unMap g, v <- vs]
--     targets name =
--       case mapMaybe (\n -> (,n) <$> (g |? n)) ([id, ('%' :), ('&' :)] <*> pure name) of
--         [] -> ([], "rx")
--         (ts, fullName) : _ -> (ts, fullName)
--     go Empty _ allConditions = Or <$> allConditions
--     go ((current, signal) :<| stack) conditions allConditions =
--       traceShow (current, size stack) $
--         let (ts, fullName) = targets current
--             stack' = stack >< mkSeq ts
--          in case fullName of
--               "broadcaster" -> go (stack >< mkSeq [(t, False) | t <- ts]) (Val 1) (allConditions |. ("broadcaster", [Val 1]))
--               '%' : _ -> go (if signal then stack else stack' (dubble conditions) (allConditions |~ (fullName, ((dubble conditions) :)))
--               '&' : _ -> go stack' (Not (And (LazyVal <$> gR |! fullName))) (allConditions |~ (fullName, ((Not (And (LazyVal <$> gR |! fullName))) :)))
--               _ -> go stack' conditions allConditions

-- part2'' =
  -- $(input 20)
    -- |- ( mkMap
           -- <$> ( many1
                   -- ( (,)
                       -- <$> (many1 (oneOf "%&" <|> alphaNum) <* string " -> ")
                       -- <*> (many1 alphaNum `sepBy1` string ", " <* eol)
                   -- )
                   -- <* eof
               -- )
       -- )
    -- -- & pushUntil
    -- -- & lowWhen
    -- & pushButton' 3
    -- & (\(_, _, _, _, _, allConditions) -> traceShowId $ Or <$> allConditions)
    -- & (\g -> eval g (g |! "rx"))
