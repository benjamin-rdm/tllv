{-# LANGUAGE DataKinds, StandaloneKindSignatures, GADTs, TypeFamilies #-}

module Vector where
    
import Prelude hiding ( head, tail, last, init )
import Data.Kind ( Type )

data Nat = Zero | Succ Nat

type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

instance Eq a => Eq (Vec n a) where
    Nil       == Nil       = True
    (x :> xs) == (y :> ys) = x == y && xs == ys

instance Functor (Vec n) where
    fmap f (x :> xs) = f x :> fmap f xs
    fmap _ Nil       = Nil

instance Show a => Show (Vec n a) where
    show Nil = "Nil"
    show (x :> xs) = show x ++ " :> " ++ show xs


head :: Vec (Succ n) a -> a
head (x :> _) = x

tail :: Vec (Succ n) a -> Vec n a
tail (_ :> xs) = xs

last :: Vec (Succ n) a -> a
last (x :> Nil) = x
last (_ :> xs) = case xs of x' :> xs' -> last xs

init :: Vec (Succ n) a -> Vec n a
init (_ :> Nil) = Nil
init (x :> xs) = case xs of x' :> xs' -> x :> init xs

type Three = Succ (Succ (Succ Zero))

example1 :: Num a => Vec Three a
example1 = 1 :> 2 :> 3 :> Nil