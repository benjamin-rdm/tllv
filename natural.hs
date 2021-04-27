{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators,
             UndecidableInstances
#-}


module Natural where

data Nat = Zero | Succ Nat deriving Show

instance Eq Nat where
    n == m = compare n m == EQ

instance Ord Nat where
    compare (Succ n) (Succ m) = compare n m
    compare Zero     Zero     = EQ
    compare Zero     _        = LT
    compare _        _        = GT

instance Num Nat where
    Zero     + y = y
    (Succ x) + y = Succ (x + y)
    Zero     * y = Zero
    (Succ x) * y = y + (x * y)
    abs = id
    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n-1))
    (Succ n) - (Succ m) = n - m
    n        - Zero     = n
    signum Zero = 0
    signum _    = 1

type family (x :: Nat) + (y :: Nat) where
    Zero     + y = y
    (Succ x) + y = Succ (x + y)

type family (x :: Nat) :* (y :: Nat) where
    Zero     :* y = Zero
    (Succ x) :* y = y + (x :* y)