module Day13 (part1, part2) where

reflects :: ArrayGrid DotHash -> IO [Either Int Int]
reflects g = do
  let valid :: ArrayGrid DotHash -> Int -> IO Bool
      valid g i = do
        (maxX, _) <- maxXYM g
        let w = min i (maxX - i + 1)
        (l, r) <- partitionCoordsM (< (i, 0)) g
        r' <- mapCoordsM (first (subtract i)) r
        l' <- cropXM (i - w) i l
        r'' <- cropXM 0 w r'
        lL <- length <$> coordsM l'
        rL <- length <$> coordsM r''
        let (a, b) = if lL < rL then (l', r'') else (r'', l')
        b' <- v0 <$> variantsM b
        return $ a == b'
  v <- r270 <$> variantsM g
  mconcat
    <$> ( forM
            [(Left, g), (Right, v)]
            ( \(f, g) -> do
                (maxX, _) <- maxXYM g
                catMaybes
                  <$> ( forM
                          [1 .. maxX]
                          ( \i -> do
                              yes <- valid g i
                              return if yes then Just (f i) else Nothing
                          )
                      )
            )
        )

reflectsSmudged :: ArrayGrid DotHash -> IO [Either Int Int]
reflectsSmudged g = do
  o <- mkSet <$> reflects g
  cs <- coordsM g
  gs <- sequence [g <||~> (c, (bool Dot Hash . (== Dot))) | c <- cs]
  ess <- traverse reflects gs
  return $ filter (∉ o) (nub $ mconcat ess)

solve :: (ArrayGrid DotHash -> IO [Either Int Int]) -> [ArrayGrid DotHash] -> IO Int
solve f gs = do
  es <- mconcat <$> traverse f gs
  return $ es & partitionEithers & both sum & second (* 100) & uncurry (+)

part1 :: Int
part1 = unsafePerformIO $ solve reflects =<< $(gridsM input 13)

part2 :: Int
part2 = unsafePerformIO $ solve reflectsSmudged =<< $(gridsM input 13)