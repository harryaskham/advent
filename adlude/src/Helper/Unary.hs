{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helper.Unary where

import Data.Foldable qualified as F

data UnaryFiller = UnaryFiller

-- Filler for evaluating prefix ops
λ :: UnaryFiller
λ = UnaryFiller

-- Type for enabling unary prefix ops that take () as a first argument.
type Unary f = UnaryFiller -> f

-- Lift a regular value to a unary context
ꟶ :: a -> Unary a
ꟶ a UnaryFiller = a

-- Partially applied u which resolves to a
class IsUnary u a where
  unary :: u -> a
  (⁻) :: u -> a
  (⁻) = unary

instance IsUnary (Unary a) a where
  unary f = f UnaryFiller

instance {-# INCOHERENT #-} IsUnary a a where
  unary = id

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Eq a) => Eq u where
  a == b = (unary a :: a) == (unary b :: a)

mkUnary1 :: IsUnary u a => (a -> b) -> Unary (u -> b)
mkUnary1 f _ = f . unary

mkUnary2 :: (IsUnary u a, IsUnary v b) => (a -> b -> c) -> Unary (u -> v -> c)
mkUnary2 f _ u v = f (unary u) (unary v)

(¬) :: IsUnary u Bool => Unary (u -> Bool)
(¬) = mkUnary1 not

infixr 3 ∧

(∧) :: IsUnary u Bool => Unary (u -> u -> Bool)
(∧) = mkUnary2 (&&)

infixr 2 ∨

(∨) :: IsUnary u Bool => Unary (u -> u -> Bool)
(∨) = mkUnary2 (||)

(⋀) :: Foldable t => Unary (t Bool -> Bool)
_ ⋀ bs = and bs

(⋁) :: Foldable t => Unary (t Bool -> Bool)
_ ⋁ bs = or bs

(∑) :: (Foldable f, Num a) => Unary (f a -> a)
(∑) = const $ F.foldl' (+) 0

(∏) :: (Foldable f, Num a) => Unary (f a -> a)
(∏) = const $ F.foldl' (*) 1
