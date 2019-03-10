-------------------------------------------------------------
--
-- Author: Joao H de A Franco (jhafranco@acm.org)
--
-- Description: FFT implementation in Haskell
--
-- Date: 2013-Mar-28
--
-- License: Attribution-NonCommercial-ShareAlike 3.0 Unported
--          (CC BY-NC-SA 3.0)
--
-------------------------------------------------------------
module Aux.FFT (fft,ifft) where
import Data.Complex(Complex((:+)))

fft, ifft :: [Complex Float] -> [Complex Float]
fft = fft' 1
ifft = fft' (-1)

fft' :: Float -> [Complex Float] -> [Complex Float]
fft' e xs = if pow2 (length xs) 
            then let z = sqrt (1 / fromIntegral (length xs))
                  in map ((z :+ 0) *) $ fft'' e xs
            else error "Number of points is not a power of 2"
            where pow2 n
                     | n == 1 || n == 2 = True
                     | otherwise = n `mod` 2 == 0 && pow2 (n `div` 2)

fft'' :: Float -> [Complex Float] -> [Complex Float]
fft'' _ [] = []
fft'' _ [x] = [x]
fft'' e xs = fft'' e (evens xs) <+> t (fft'' e (odds xs))
  where (<+>) r s = zipWith (+) (r ++ r) (s ++ map negate s)
        evens [] = []
        evens [u] = [u]
        evens (v:_:vs) = v:evens vs
        odds = evens . drop 1
        n = 2 * pi / fromIntegral (length xs)
        t = zipWith (\k z -> z * exp (0 :+ k * n * e)) ([0..] :: [Float])



