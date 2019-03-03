module ComonadTest where

-- from http://blog.sigfpe.com/2007/01/monads-hidden-behind-every-zipper.html

data Zipper a =
  Zipper [a] [a]
  deriving (Eq, Show)

left :: Zipper a -> Zipper a
left (Zipper (a:as) bs) = Zipper as (a : bs)

right :: Zipper a -> Zipper a
right (Zipper as (b:bs)) = Zipper (b : as) bs

class Comonad w where
  coreturn :: w a -> a
  cobind :: (w a -> b) -> w a -> w b

iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = tail $ iterate f x

instance Comonad Zipper where
  cobind f a = f <$> Zipper (iterate1 left a) (iterate right a)
  coreturn (Zipper _ (b:_)) = b

a :: Zipper Integer
a = Zipper (repeat 0) ([0, 1, 2, 3] ++ repeat 0)

f :: Num a => Zipper a -> a
f (Zipper (a:_) (b:c:_)) = a + 2 * b + c

test :: [Integer]
test =
  let Zipper u v = cobind f a
   in take 5 v

plus :: Num a => Zipper a -> Zipper a -> Zipper a
plus (Zipper a b) (Zipper a' b') = Zipper (plus' a a') (plus' b b')
  where
    plus' (a:as) (b:bs) = (a + b) : plus' as bs
    plus' a [] = a
    plus' [] a = a

left' :: Num a => Zipper a -> Zipper a
left' (Zipper (a:as) bs) = Zipper as (a : bs)
left' (Zipper [] bs) = Zipper [] (0 : bs)

right' :: Num a => Zipper a -> Zipper a
right' (Zipper as (b:bs)) = Zipper (b : as) bs
right' (Zipper as []) = Zipper (0 : as) []

tail' :: [a] -> [a]
tail' [] = []
tail' a = tail a

stagger :: (a -> a) -> [a] -> [a]
stagger f [] = []
stagger f (x:xs) = x : map f (stagger f xs)

stagger1 :: (a -> a) -> [a] -> [a]
stagger1 f x = tail' (stagger f x)

instance Functor Zipper where
  fmap f (Zipper a b) = Zipper (map f a) (map f b)

return' :: a -> Zipper a
return' a = Zipper [] [a]

bind' :: Num a => (b -> Zipper a) -> Zipper b -> Zipper a
bind' f x =
  let Zipper a b = fmap f x
   in foldl1 plus (stagger left' b ++ stagger1 right' a)

a' :: Zipper Integer
a' = Zipper [] [0, 1, 2, 3]

f' :: Num a => a -> Zipper a
f' x = Zipper [x] [2 * x, x]

test' :: [Integer]
test' =
  let Zipper u v = bind' f' a'
   in take 5 v
