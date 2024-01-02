{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Helper.Unary where

import Data.Foldable qualified as F

ȣ :: ()
ȣ = ()

-- Type for enabling unary prefix ops that take a placeholder as first argument.
type Unary a = () -> a

ɾ :: a -> Unary a
ɾ a _ = a

-- Partially applied u which resolves to a
class IsUnary u a where
  -- Resolve the thunk
  unary :: u -> a

  -- Postfix forcing
  (⁻) :: u -> a
  (⁻) = unary

  -- Prefix forcing
  λ :: u -> a
  λ = unary

  -- Map forcing
  ƛ :: Functor f => f u -> f a
  ƛ = fmap unary

instance IsUnary (Unary a) a where
  unary f = f ()

instance {-# INCOHERENT #-} IsUnary a a where
  unary = id

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Eq a) => Eq u where
  a == b = (unary a :: a) == (unary b :: a)

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Ord a) => Ord u where
  a <= b = (unary a :: a) <= (unary b :: a)

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Semigroup a) => Semigroup u where
  a <> b = ɾ ((unary a :: a) <> (unary b :: a))

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Monoid a) => Monoid u where
  mempty = ɾ (mempty :: a)

mkUnary1 :: (IsUnary u a) => (a -> b) -> Unary (u -> b)
mkUnary1 f _ u = f (unary u)

mkUnary2 :: (IsUnary u a, IsUnary v b) => (a -> b -> c) -> u -> v -> Unary c
mkUnary2 f u v _ = f (unary u) (unary v)

infix 9 ¬

(¬) :: IsUnary u Bool => Unary (u -> Bool)
(¬) = mkUnary1 not

infixr 3 ∧

(∧) :: IsUnary u Bool => u -> u -> Unary Bool
(∧) = mkUnary2 (&&)

infixr 2 ∨

(∨) :: IsUnary u Bool => u -> u -> Unary Bool
(∨) = mkUnary2 (||)

infix 9 ⋀

(⋀) :: (Foldable t, Functor t) => Unary (t Bool -> Bool)
(⋀) _ = and . fmap unary

infix 9 ⋁

(⋁) :: (Foldable t, Functor t) => Unary (t Bool -> Bool)
(⋁) _ = or

infix 9 ∑

(∑) :: (Foldable t, Functor t, Num a) => Unary (t a -> a)
(∑) _ = sum

infix 9 ∏

(∏) :: (Foldable t, Functor t, Num a) => Unary (t a -> a)
(∏) _ = product

testUnary :: Bool
testUnary =
  let a :: Unary Int
      a = (∏ [2 .. 4 :: Int])
      b :: Bool
      b = a == ɾ 24
      c :: Bool
      c = λ ((∑ [1 .. 10 :: Int]) > ɾ 100)
      d :: Unary Bool
      d = (¬ c)
      e :: Unary Bool
      e = ɾ b ∨ d
      f :: Unary Bool
      f = (⋀ ƛ [ɾ b, d, e])
   in λ f
