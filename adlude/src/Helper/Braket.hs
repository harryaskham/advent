module Helper.Braket where

data BraketDivider = Ⲓ

data BraketClose = BraketClose

ᐳ :: BraketClose
ᐳ = BraketClose

ᕽ :: BraketClose
ᕽ = BraketClose

class Braket t a b where
  く :: a -> BraketDivider -> b -> BraketClose -> t a b
  unBraket :: t a b -> (a, b)

instance Braket (,) a b where
  く a Ⲓ b _ = (a, b)
  unBraket = id

a :: (Int, Int)
a = く 1 Ⲓ 2 ᐳ
