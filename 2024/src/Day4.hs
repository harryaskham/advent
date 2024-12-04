module Day4 (part1, part2) where

-- occurrences :: forall n. ((MakeVariadicConcat n Text (Σ ℤ)) => MakeVariadicF n Text (Σ ℤ))
occurrences :: forall n. (VarC n Text (Σ ℤ)) => Var n (Text *-> Σ ℤ)
occurrences = makeVariadicConcat @n $ \kernel ->
  cells
    |=< convolveWith ((Σ . bool @ℤ 0 1) .<. x_x)
    <$> rotations (readGrid kernel)
    <*> [readGrid @HashGrid' $(exampleInput 4)]

part1 :: Σ ℤ
part1 =
  ( occurrences @2
      ([txt|XMAS|] :: Text)
      ( [txt|X___
           _M__
           __A_
           ___S|] ::
          Text
      )
  )

part2 :: Σ ℤ
part2 =
  ( occurrences @1
      ( [txt|M_S
           _A_
           M_S|] ::
          Text
      )
  )
