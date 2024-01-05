{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module Helper.Braket where

import Helper.Alias

data Braket a b = Braket a b

newtype Bra' a = Bra' a

newtype Ket a = Ket a

-- To enable using partially applied bra-kets as bras
type Bra a = () -> BraketClose -> Braket a ()

data BraketDivider = Ⲓ

data BraketClose = BraketClose

ᐳ :: BraketClose
ᐳ = BraketClose

class Braketed t a b where
  く :: a -> BraketDivider -> b -> BraketClose -> t a b
  unBraket :: t a b -> (a, b)
  unBra :: t a b -> a
  unBra = fst . unBraket
  unKet :: t a b -> b
  unKet = snd . unBraket

instance Braketed Braket a b where
  く a Ⲓ b ᐳ = Braket a b
  unBraket (Braket a b) = (a, b)

instance Braketed (,) a b where
  く a Ⲓ b ᐳ = (a, b)
  unBraket = id

class BraketedBra t a where
  bra :: t -> Bra' a

instance BraketedBra (Bra' a) a where
  bra = id

instance (Braketed t a b) => BraketedBra (b -> BraketClose -> t a b) a where
  bra b = Bra' . fst . unBraket $ b (⊥) ᐳ

class BraketedKet t a where
  ket :: t -> Ket a

ߊ :: a -> BraketClose -> Ket a
ߊ a ᐳ = Ket a

instance BraketedKet (Ket a) a where
  ket = id

a :: Braket Int Int
a = く 1 Ⲓ 2 ᐳ

b :: Bra Int
b = く 1 Ⲓ

c :: Ket Int
c = ߊ 2 ᐳ
