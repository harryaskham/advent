{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Helper.Unary where

import Data.Foldable qualified as F

-- Filler for evaluating prefix ops
λ :: ()
λ = ()

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
  ƛ :: u -> a
  ƛ = unary

instance IsUnary (Unary a) a where
  unary f = f λ

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

(¬) :: IsUnary u Bool => Unary (u -> Bool)
(¬) = mkUnary1 not

infixr 3 ∧

(∧) :: IsUnary u Bool => u -> u -> Unary Bool
(∧) = mkUnary2 (&&)

infixr 2 ∨

(∨) :: IsUnary u Bool => u -> u -> Unary Bool
(∨) = mkUnary2 (||)

(⋀) :: (Foldable t, Functor t, IsUnary u Bool) => Unary (t u -> Bool)
(⋀) _ = and . fmap unary

(⋁) :: (Foldable t, Functor t, IsUnary u Bool) => Unary (t u -> Bool)
(⋁) _ = or . fmap unary

(∑) :: (Foldable t, Functor t, Num a, IsUnary u a) => Unary (t u -> a)
(∑) _ = F.foldl' (+) 0 . fmap unary

(∏) :: (Foldable t, Functor t, Num a, IsUnary u a) => Unary (t u -> a)
(∏) _ = F.foldl' (*) 1 . fmap unary

testUnary :: Bool
testUnary =
  let a :: Unary Int
      a = (∏ [2 .. 4 :: Int])
      b :: Bool
      b = a == ɾ 24
      c :: Unary Int
      c = (∑ [1 .. 10 :: Int])
      d :: Bool
      d = c > ɾ 100
      e :: Unary Bool
      e = (¬ d)
      f :: Unary Bool
      f = ɾ b ∨ e
      g :: Unary Bool
      g = (⋀ [ɾ b, e, f])
   in ƛ g
