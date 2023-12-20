module Day20 (part1, part2) where

mkInitialState :: Map String [String] -> (Int, Int, [Map String (Map String Bool, Bool)], Bool)
mkInitialState g =
  let gR = mkMapWith (<>) [(v, [k]) | (k, vs) <- unMap g, v <- vs]
      initialInputs "broadcaster" = mkMap []
      initialInputs ('%' : _) = mkMap []
      initialInputs ('&' : name) = mkMap [(inputName, False) | inputName <- gR |! name]
      s = mkMap [(name, (initialInputs name, False)) | name <- keys g]
   in (0, 0, [s], False)

pushButton :: Int -> Map String [String] -> (Int, Int, [Map String (Map String Bool, Bool)], Bool)
pushButton n g =
  let (low, high, ss, satisfied) = iterate (onePass g) (mkInitialState g) !! n in (n + low, high, reverse ss, satisfied)

onePass :: Map String [String] -> (Int, Int, [Map String (Map String Bool, Bool)], Bool) -> (Int, Int, [Map String (Map String Bool, Bool)], Bool)
onePass g st = go st (mkSeq [("button", "broadcaster", False)])
  where
    targets :: String -> Maybe ([String], String)
    targets name =
      -- traceShow (name) $
      case mapMaybe (\n -> (,n) <$> (g |? n)) ([id, ('%' :), ('&' :)] <*> pure name) of
        [] -> Nothing
        (ts, fullName) : _ -> Just (ts, fullName)
    go :: (Int, Int, [Map String (Map String Bool, Bool)], Bool) -> Seq (String, String, Bool) -> (Int, Int, [Map String (Map String Bool, Bool)], Bool)
    go st Empty = st
    go (low, high, ss@(s : _), satisfied) (event@(origin, current, incomingSignal) :<| stack) =
      -- traceShow (st, event) $
      -- traceShow (event) $
      let Just (ts, fullName) = targets current
          (inputs, signal) = s |! fullName
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
                  Just False -> (low + length ts, high, (s' |~ (fullName, second (const False))) : ss, satisfied')
                  Just True -> (low, high + length ts, (s' |~ (fullName, second (const True))) : ss, satisfied')
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

pushUntil :: Map String [String] -> Int
pushUntil g = go 0 (mkInitialState g)
  where
    go n (_, _, _, True) = n
    go n st = traceShow n $ go (n + 1) (onePass g st)

nameToHistory :: Int -> Map String [String] -> String -> [Bool]
nameToHistory n g name = let (_, _, history, _) = pushButton n g in (\h -> snd (h |! name)) <$> history

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
    go ("broadcaster", True) = error "true broadcaster"
    go (name, signal) =
      -- traceShow (name, gR) $
      case targets name of
        Nothing -> mconcat [go (precursor, signal) | precursor <- gR |! dropWhile (∈ ("%&" :: String)) name]
        Just (_, fullName@('&' : _)) -> mconcat [go (drop 1 precursor, not signal) | precursor <- gR |! dropWhile (∈ ("%&" :: String)) name]
        Just (_, fullName@('%' : _)) -> [(fullName, signal)]

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

part2 :: Int
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
    -- & pushUntil
    & ( \g ->
          ( first
              ( \name ->
                  let h = nameToHistory 10000 g name
                      pairs = zip [0 ..] $ zip h (drop 1 h)
                      changes = [p | p@(i, (a, b)) <- pairs, a /= b]
                   in (name, changes)
              )
          )
            <$> requirements g ("rx", False)
      )
    & traceShowId
    & length
