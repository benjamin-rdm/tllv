{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators,
             UndecidableInstances
#-}


module Natural where

data Nat = Zero | Succ Nat

type family (x :: Nat) + (y :: Nat) where
    Zero     + y = y
    (Succ x) + y = Succ (x + y)

type family (x :: Nat) :* (y :: Nat) where
    Zero     :* y = Zero
    (Succ x) :* y = y + (x :* y)