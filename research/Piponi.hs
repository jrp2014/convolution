module Pipone where

-- http://blog.sigfpe.com/2007/01/monads-hidden-behind-every-zipper.html
data Zipper a =
  Zipper [a] [a]
  deriving (Eq, Show)

instance Functor Zipper where
  fmap f (Zipper a b) = Zipper (map f a) (map f b)

left (Zipper (a:as) bs) = Zipper as (a : bs)

right (Zipper as (b:bs)) = Zipper (b : as) bs

class Comonad w where
  coreturn :: w a -> a
  cobind :: (w a -> b) -> w a -> w b

iterate1 f x = tail $ iterate f x

instance Comonad Zipper where
  cobind f a = fmap f $ Zipper (iterate1 left a) (iterate right a)
  coreturn (Zipper _ (b:_)) = b

a = Zipper (repeat 0) ([0, 1, 2, 3] ++ repeat 0)

f (Zipper (a:_) (b:c:_)) = a + 2 * b + c

test =
  let Zipper u v = cobind f a
   in take 5 v
