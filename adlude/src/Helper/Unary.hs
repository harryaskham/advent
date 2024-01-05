{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Helper.Unary where

import Data.Foldable qualified as F

-- A class that enables application of datatypes
class UnaryApply a b c where
  (˙) :: a -> b -> c

-- Intersections

class Intersectable a where
  (∩) :: a -> a -> a
  (⋂) :: (Foldable f) => Unary (f a -> a)
  (⋂) = const $ F.foldl1 (∩)

data UnaryIntersect = Ⴖ

instance (Foldable f, Intersectable a) => UnaryApply UnaryIntersect (f a) a where
  (˙) Ⴖ = F.foldl1 (∩)

-- Unions

class Unionable a where
  (∪) :: a -> a -> a
  (⋃) :: (Foldable f) => Unary (f a -> a)
  (⋃) = const $ F.foldl1 (∪)

data UnaryUnion = Ս

instance (Foldable f, Unionable a) => UnaryApply UnaryUnion (f a) a where
  (˙) Ս = F.foldl1 (∪)

-- N-ary boolean operations

data UnaryFoldNum = Σ | Π

instance forall a (f :: Type -> Type). (Foldable f, Num a) => UnaryApply UnaryFoldNum (f a) a where
  (˙) Σ = sum
  (˙) Π = product

ⵉ :: forall a (f :: Type -> Type). (Foldable f, Num a) => f a -> a
ⵉ = sum

ꛛ :: forall a (f :: Type -> Type). (Foldable f, Num a) => f a -> a
ꛛ = product

-- Negation

data UnaryNot = Ⴈ

instance UnaryApply UnaryNot Bool Bool where
  (˙) Ⴈ = not

ⴈ :: Bool -> Bool
ⴈ = not

-- Folding

data UnaryFold a b = Ł (b -> a -> b) b | Ɍ (a -> b -> b) b

instance forall a b (f :: Type -> Type). (Foldable f) => UnaryApply (UnaryFold a b) (f a) b where
  (˙) (Ł f b) = F.foldl' f b
  (˙) (Ɍ f b) = F.foldr f b

data UnaryFold1 a = Ŀ (a -> a -> a) | Ṛ (a -> a -> a)

instance forall a (f :: Type -> Type). (Foldable f) => UnaryApply (UnaryFold1 a) (f a) a where
  (˙) (Ŀ f) = F.foldl1 f
  (˙) (Ṛ f) = F.foldr1 f

-- Unary forcing

data UnaryForce = Λ

instance UnaryApply UnaryForce (Unary a) a where
  (˙) Λ = ($ ())

-- Below here: experimental use of operators in prefix mode using a placeholder on the left side.

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

(¬) :: (IsUnary u Bool) => Unary (u -> Bool)
(¬) = mkUnary1 not

infixr 3 ∧

(∧) :: (IsUnary u Bool) => u -> u -> Unary Bool
(∧) = mkUnary2 (&&)

infixr 2 ∨

(∨) :: (IsUnary u Bool) => u -> u -> Unary Bool
(∨) = mkUnary2 (||)

infix 9 ⋀

(⋀) :: (Foldable t, Functor t) => Unary (t Bool -> Bool)
(⋀) _ = and

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
      f = (⋀ fmap λ [ɾ b, d, e])
   in λ f
