module Day20 (part1, part2) where

mkInitialState :: Map String [String] -> (ℤ', ℤ', [Map String (Map String Bool, [(ℤ', Bool)])], Bool)
mkInitialState g =
  let gR = mkMapWith (<>) [(v, [k]) | (k, vs) <- unMap g, v <- vs]
      initialInputs "broadcaster" = mkMap []
      initialInputs ('%' : _) = mkMap []
      initialInputs ('&' : name) = mkMap [(inputName, False) | inputName <- gR |! name]
      s = mkMap [(name, (initialInputs name, [(0, False)])) | name <- keys g]
   in (0, 0, [s], False)

pushButton :: ℤ' -> Map String [String] -> (ℤ', ℤ', [Map String (Map String Bool, [(ℤ', Bool)])], Bool)
pushButton n' g =
  let go n st
        | n == n' = st
        | otherwise = go (n + 1) (onePass n g st)
   in let (low, high, ss, satisfied) = go 0 (mkInitialState g) in (n' + low, high, reverse ss, satisfied)

onePass :: ℤ' -> Map String [String] -> (ℤ', ℤ', [Map String (Map String Bool, [(ℤ', Bool)])], Bool) -> (ℤ', ℤ', [Map String (Map String Bool, [(ℤ', Bool)])], Bool)
onePass n g st = go st (mkSeq [("button", "broadcaster", False)])
  where
    targets :: String -> Maybe ([String], String)
    targets name =
      case mapMaybe (\n -> (,n) <$> (g |? n)) ([id, ('%' :), ('&' :)] <*> pure name) of
        [] -> Nothing
        (ts, fullName) : _ -> Just (ts, fullName)
    go st Empty = st
    go (low, high, ss@(s : _), satisfied) ((origin, current, incomingSignal) :<| stack) =
      let Just (ts, fullName) = targets current
          (inputs, (_, signal) : _) = s |! fullName
          inputs' = inputs |. (origin, incomingSignal)
          signal' = case fullName of
            "broadcaster" -> Just False
            ('%' : _) -> if incomingSignal then Nothing else Just (λ ¬ signal)
            ('&' : _) -> if all snd (unMap inputs') then Just False else Just True
          s' = s |~ (fullName, first (const inputs'))
          st' =
            let satisfied' = satisfied || ("rx" :: String) ∈ (ts :: [String]) && signal' == Just False
             in case signal' of
                  Nothing -> (low, high, s' : ss, satisfied')
                  Just False -> (low + length ts, high, (s' |~ (fullName, second ((n, False) :))) : ss, satisfied')
                  Just True -> (low, high + length ts, (s' |~ (fullName, second ((n, True) :))) : ss, satisfied')
       in go st' $ case signal' of
            Nothing -> stack
            Just signalToSend -> stack >< mkSeq [(fullName, t, signalToSend) | t <- ts, isJust (targets t)]

nameToHistory :: ℤ' -> Map String [String] -> String -> [(ℤ', Bool)]
nameToHistory n g name = let (_, _, history, _) = pushButton n g in reverse $ ulast history `get` name
  where
    get h n = snd . uhead $ mapMaybe (h |?) ([id, ('%' :), ('&' :)] <*> pure n)

stateChanges :: Map String [String] -> [[((ℤ', Bool), (ℤ', Bool))]]
stateChanges g =
  ( \name ->
      let h = nameToHistory 5000 g name
          pairs = zip h (drop 1 h)
       in [p | p@((_, a), (_, b)) <- pairs, a /= b]
  )
    <$> (let gR = mkMapWith (<>) [(v, [k]) | (k, vs) <- unMap g, v <- vs] in gR |! "vf")

circuit :: Map String [String]
circuit =
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

part1 :: ℤ'
part1 =
  circuit
    & pushButton 1000
    & (\(low, high, _, _) -> (low, high))
    & uncurry (*)

part2 :: ℤ'
part2 =
  circuit
    & stateChanges
    & fmap ((snd >>> fst >>> (+ 1)) . uhead)
    & (λ ∏)
