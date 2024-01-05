module Helper.Alias where

import Data.Complex (Complex)

(⊥) :: a
(⊥) = error "Reached ⊥"

type ℤ' = Int

type ℤ = Integer

type ℕ = Natural

type ℚ = Rational

type ℝ = Double

type ℂ = Complex ℝ

type 𝔹 = Bool
