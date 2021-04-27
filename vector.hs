{-# LANGUAGE DataKinds, StandaloneKindSignatures, GADTs, TypeFamilies,
             FlexibleInstances, MultiParamTypeClasses, TypeOperators,
             UndecidableInstances
#-}

module Vector where
    
import Prelude ( Num, Show, Functor, Eq, show, fmap, (==), (&&),
                 Bool( True ), Applicative, (<*>), pure )
import qualified Prelude as P
import Data.Kind ( Type )

data Nat = Zero | Succ Nat

type family (x :: Nat) + (y :: Nat) where
    Zero     + y = y
    (Succ x) + y = Succ (x + y)

type family (x :: Nat) :* (y :: Nat) where
    Zero     :* y = Zero
    (Succ x) :* y = y + (x :* y)

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
    show Nil       = "Nil"
    show (x :> xs) = P.concat [show x, " :> ", show xs]

instance Repeat n => Applicative (Vec n) where
    pure = repeat
    fs <*> xs = zipWith (\f x -> f x) fs xs

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

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) (x :> xs) ys = x :> (xs ++ ys)
(++) Nil       ys = ys

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (a :> as) (b :> bs) = f a b :> zipWith f as bs

zip :: Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)

concat :: Vec n (Vec m a) -> Vec (n :* m) a
concat Nil       = Nil
concat (x :> xs) = x ++ concat xs

snoc :: a -> Vec n a -> Vec (Succ n) a
snoc a Nil       = a :> Nil
snoc a (x :> xs) = x :> snoc a xs

class Repeat (n :: Nat) where
    repeat :: a -> Vec n a

instance Repeat Zero where
    repeat _ = Nil

instance Repeat n => Repeat (Succ n) where
    repeat a = a :> repeat a

class Reverse (n :: Nat) where
    reverse :: Vec n a -> Vec n a

instance Reverse Zero where
    reverse Nil = Nil

instance Reverse n => Reverse (Succ n) where
    reverse (x :> xs) = snoc x (reverse xs)

type Three = Succ (Succ (Succ Zero))

example1 :: Num a => Vec Three a
example1 = 1 :> 2 :> 3 :> Nil