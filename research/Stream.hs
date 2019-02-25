module Stream where

-- https://www.cs.ox.ac.uk/ralf.hinze/publications/CSC.pdf
--
import qualified Data.Stream                   as S
import           Data.Stream                    ( Stream(..)
                                                , (<:>)
                                                )

import           Control.Comonad

instance (Num a) => Num (Stream a) where
  (+)         = S.zipWith (+)
  (-)         = S.zipWith (-)
  (*)         = S.zipWith (*)
  negate      = S.map negate
  abs         = S.map abs
  signum      = S.map signum
  fromInteger = S.repeat . fromInteger

instance Comonad Stream where
  extract = S.head
  extend f s = f s <:> extend f (S.tail s)
  duplicate s = s <:> duplicate (S.tail s)

sumS :: Num a => Int -> Stream a -> a
sumS n s | n <= 0    = 0
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
conv :: Num a => [a] -> [a] -> [a]
conv s t = conv' (s ++ replicate (ll - ls) 0) (t ++ replicate (ll - lt) 0)
 where
  ls = length s
  lt = length t
  ll = ls + lt - 1

conv' :: Num a => [a] -> [a] -> [a]
conv' (hs : ts) t'@(ht : tt) =
  hs * ht : zipWith (+) (map (hs *) tt) (conv' ts t')

-- conv h f == g
h, f, g :: [Int]
h = [-8, -9, -3, -1, -6, 7]

f = [-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]

g =
  [ 24
  , 75
  , 71
  , -34
  , 3
  , 22
  , -45
  , 23
  , 245
  , 25
  , 52
  , 25
  , -67
  , -96
  , 96
  , 31
  , 55
  , 36
  , 29
  , -43
  , -7
  ]
