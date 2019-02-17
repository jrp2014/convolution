module Stream where

-- https://www.cs.ox.ac.uk/ralf.hinze/publications/CSC.pdf
--
import qualified Data.Stream as S
import Data.Stream (Stream(..), (<:>))

import Control.Comonad

instance (Num a) => Num (Stream a) where
  (+) = S.zipWith (+)
  (-) = S.zipWith (-)
  (*) = S.zipWith (*)
  negate = S.map negate
  abs = S.map abs
  signum = S.map signum
  fromInteger = S.repeat . fromInteger

instance Comonad Stream where
  extract = S.head
  extend f s = f s <:> extend f (S.tail s)
  duplicate s = s <:> duplicate (S.tail s)

sumS :: Num a => Int -> Stream a -> a
sumS n s
  | n <= 0 = 0
  | otherwise = S.head s + sumS (n - 1) (S.tail s)

average :: Fractional a => Int -> Stream a -> a
average n stm = sumS n stm / fromIntegral n

movingAvg :: Fractional a => Int -> Stream a -> Stream a
movingAvg n = extend (average n)

convolve :: Num a => Stream a -> Stream a -> Stream a
convolve s t =
  S.head s * S.head t <:> S.repeat (S.head s) * S.tail t + convolve (S.tail s) t

-- s and t must be of the same length

-- convolve' [1,2,3,4,0,0,0,0] [1,2,3,4,5,0,0,0] == [1,4,10,20,30,34,31,20]
-- works with infinite lists; needs to be padded for finite ones
convolve' :: Num a => [a] -> [a] -> [a]
convolve' (hs:ts) t@(ht:tt) = hs * ht : zipWith (+) (zipWith (*) (repeat hs) tt) 
                                                    (convolve' ts t)

