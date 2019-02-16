{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ComonadicConvolutions where

-- from http://blog.ezyang.com/2010/02/comonads-and-convolutions/
import           Control.Comonad
import           Data.List
import           Control.Applicative

newtype Causal a =
  Causal [a]
  deriving (Show, Functor, Applicative)

-- instance Functor Causal where
--  fmap f (Causal xs) = Causal $ map f xs

-- instance Applicative Causal where
--   pure x = Causal [x]
--   (Causal xs) <*> (Causal ys) = Causal (xs <*> ys)

causal :: a -> [a] -> Causal a
causal x xs = Causal (x : xs)

unCausal :: Causal a -> [a]
unCausal (Causal xs) = xs

type Voltage = Float

disp :: Int -> Causal a -> [a]
disp n (Causal xs) = Data.List.take n xs

ltiChannel :: [Voltage] -> Causal Voltage -> Voltage
ltiChannel u (Causal f) = sum $ zipWith (*) (reverse f) u

tiChannel' :: ([Voltage] -> Voltage) -> Causal Voltage -> Voltage
tiChannel' f (Causal xs) = f (reverse xs ++ repeat 0)

ltiChannel' :: [Voltage] -> Causal Voltage -> Voltage
ltiChannel' u = tiChannel' (sum . zipWith (*) u)

ltiChannel'' :: [Voltage] -> Causal Voltage -> Voltage
ltiChannel'' u = tiChannel' (sum . liftA2 (*) u)

usr :: [Voltage]
usr = [1 .. 4]

instance Comonad Causal where
  extend f = Causal . map (f . Causal) . tail . inits . unCausal
--  duplicate = Causal . map Causal . tail . inits . unCausal
--  duplicate = extend id
  extract (Causal xs) = head xs

unitStep :: Causal Voltage
unitStep = Causal [1 .. 5]

result :: Causal Voltage
result = unitStep =>> ltiChannel' usr

result' :: Causal Voltage
result' = unitStep =>> ltiChannel' usr

-- fails to terminate
result'' :: Causal Voltage
result'' = unitStep =>> ltiChannel'' usr
