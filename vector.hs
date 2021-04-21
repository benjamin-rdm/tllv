{-# LANGUAGE DataKinds, StandaloneKindSignatures, GADTs #-}

module Vector where
import Data.Kind

data Nat = Zero | Succ Nat

type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>