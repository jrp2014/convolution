-------------------------------------------------------------
--
-- Author: Joao H de A Franco (jhafranco@acm.org)
--
-- Description: Test module for FFT implementation in Haskell
--
-- Date: 2013-Mar-28
--
-- License: Attribution-NonCommercial-ShareAlike 3.0 Unported
--          (CC BY-NC-SA 3.0)
--
-------------------------------------------------------------
module Main where

import Aux.DFT (dft, idft)
import Aux.FFT (fft, ifft)
import Data.Complex (Complex((:+)), imagPart, realPart)

-- NB: Pad to the next power of 2 to use fft
conv :: [Complex Float] -> [Complex Float] -> [Complex Float]
conv xs ys =
  take ll . map (* scale) $
  idft $ zipWith (*) (dft (xs ++ padx)) (dft (ys ++ pady))
  where
    lx = length xs
    ly = length ys
    ll = lx + ly - 1
    l = 2 * max lx ly - 1
    padx = replicate (ll - lx) 0
    pady = replicate (ll - ly) 0
    scale = sqrt . fromIntegral $ ll

main :: IO ()
main =
  print test6
    {-
  print
    (if test1 && test2 && test3
       then "ok"
       else "nok")
 -}

-- 7-point DFT (prime number) compared to Mathematica's evaluation
test1 :: Bool
test1 =
  let mathematica1 =
        [ 3.40168 :+ 0
        , (-0.566947) :+ 0.564962
        , (-0.566947) :+ (-1.41899)
        , (-0.566947) :+ 0.645967
        , (-0.566947) :+ (-0.645967)
        , (-0.566947) :+ 1.41899
        , (-0.566947) :+ (-0.564962)
        ]
   in map (roundComplex 3 0.001) (dft [0, 1, 2, 3, 0, 1, 2]) ==
      map (roundComplex 3 0.001) mathematica1

-- 8-point FFT (power of 2) compared to Mathematica's evaluation
test2 :: Bool
test2 =
  let mathematica2 =
        [ 4.24264 :+ 0
        , 0 :+ 0
        , (-1.41421) :+ (-1.41421)
        , 0 :+ 0
        , (-1.41421) :+ 0
        , 0 :+ 0
        , (-1.41421) :+ 1.41421
        , 0 :+ 0
        ]
   in map (roundComplex 3 0.001) (fft [0, 1, 2, 3, 0, 1, 2, 3]) ==
      map (roundComplex 3 0.001) mathematica2

-- 512-point DFT compared to FFT
test3 :: Bool
test3 =
  let list = take 512 $ cycle ([0, 1, 2, 3] :: [Complex Float])
      z1 = map (roundComplex 2 0.001) $ dft list
      z2 = map (roundComplex 2 0.001) $ fft list
   in z1 == z2

-- 128-point time-shifted impulse forward & inverse DFT (auto-test)
test4 :: Bool
test4 = all (== True) t4
  where
    t4 = do
      n <- [0 .. 127]
      let x = impulse n 128 :: [Complex Float]
      return $ map (roundComplex 3 0.001) (idft (dft x)) == x

-- 1024-point time-shifted impulse forward & inverse FFT (auto-test)         
test5 :: Bool
test5 = all (== True) t5
  where
    t5 = do
      n <- [0 .. 1023]
      let x = impulse n 1024 :: [Complex Float]
      return $ map (roundComplex 3 0.001) (ifft (fft x)) == x

test6 :: [Complex Float]
test6 =conv (map fromInteger [1..2^10] :: [Complex Float]) (map fromInteger [1..2^13] :: [Complex Float])

-- Discrete Dirac delta generator
impulse :: Num a => Int -> Int -> [a]
impulse n m = replicate n 0 ++ 1 : replicate (m - n - 1) 0

-- Rounding function
roundComplex :: Int -> Float -> Complex Float -> Complex Float
roundComplex n e x =
  let zeroFloat e' f =
        if abs f < e'
          then 0
          else f
      trim = roundFloat n . zeroFloat e
      roundFloat m y = fromIntegral (round $ y * 10 ^ m :: Int) / 10 ^^ m
   in trim (realPart x) :+ trim (imagPart x)
