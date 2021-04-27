{-# LANGUAGE DataKinds, GADTs, TypeFamilies, FlexibleInstances, 
             MultiParamTypeClasses, TypeOperators
#-}

module Vector where
    
import Prelude ( Num, Show, Functor, Eq, Ord, show, fmap, (==), (&&),
                 Bool( True, False ), Applicative, (<*>), pure, ($), (<$>),
                 compare, min, max, (+), Ordering (LT) )
import qualified Prelude as P
import Data.Kind ( Type )
import Control.Applicative ( liftA2 )
import Data.Foldable ( Foldable, fold, foldr, foldl, toList )

import Natural

data Vec (n :: Nat) a where
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
    (<*>) = zipWith ($)
    liftA2 = zipWith

instance Foldable (Vec n) where
    foldr f b (x :> xs) = f x (foldr f b xs)
    foldr _ b _         = b

instance Ord a => Ord (Vec n a) where
    compare xs ys = fold (zipWith compare xs ys)

foldr1, foldl1 :: (a -> a -> a) -> Vec (Succ n) a -> a
foldr1 f (x :> xs) = foldr f x xs
foldl1 f (x :> xs) = foldl f x xs

minimum, maximum :: Ord a => Vec (Succ n) a -> a
minimum = foldr1 min
maximum = foldr1 max

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

uncons :: Vec (Succ n) a -> (a, Vec n a)
uncons (x :> xs) = (x, xs)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(x :> xs) ++ ys = x :> (xs ++ ys)
Nil       ++ ys = ys

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (a :> as) (b :> bs) = f a b :> zipWith f as bs
zipWith _ Nil       Nil       = Nil

zip :: Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)

concat :: Vec n (Vec m a) -> Vec (n :* m) a
concat Nil       = Nil
concat (x :> xs) = x ++ concat xs

(<:) :: Vec n a -> a -> Vec (Succ n) a
Nil       <: a = a :> Nil
(x :> xs) <: a = x :> (xs <: a)

insert :: Ord a => a -> Vec n a -> Vec (Succ n) a
insert a Nil = a :> Nil
insert a (b :> bs) = case compare a b of 
    LT -> a :> b :> bs
    _  -> b :> insert a bs 

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
    reverse (x :> xs) = reverse xs <: x

class Iterate (n :: Nat) where
    iterate :: (a -> a) -> a -> Vec n a

instance Iterate Zero where
    iterate _ _ = Nil

instance Iterate n => Iterate (Succ n) where
    iterate f a = a :> (f <$> iterate f a)

type Three = Succ (Succ (Succ Zero))

example1, example1' :: Num a => Vec Three a
example1 = 1 :> 2 :> 3 :> Nil
example1' = iterate (+1) 1