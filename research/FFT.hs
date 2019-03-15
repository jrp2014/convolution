module FFT where

import           Data.Complex

-- https://www.lopezferrando.com/learning-haskell/

fft :: [Complex Double] -> Complex Double -> [Complex Double]
fft [a] _ = [a]
fft a   w = zipWith (+) f_even (zipWith (*) ws1 f_odd)
  ++ zipWith (+) f_even (zipWith (*) ws2 f_odd)
 where
        -- Take even and odd coefficients of a(x)
  a_even = map fst $ filter (even . snd) (zip a [0 ..])
  a_odd  = map fst $ filter (odd . snd) (zip a [0 ..])
  -- Compute FFT of a_even(x) and a_odd(x) recursively
  f_even = fft a_even (w ^ 2)
  f_odd  = fft a_odd (w ^ 2)
  -- Compute 1st and 2nd half of unity roots (ws2 = - ws1)
  ws1    = take (length a `div` 2) (iterate (* w) 1)
  ws2    = map negate ws1

-- Multiplication of polynomials: c(x) = a(x) · b(x)
--     * a(x) and b(x) have degree n (power of 2), c(x) has degree 2n
--   Algorithm:
--     1. Compute f_a = fft(a), f_b = fft(b)  .. O(n·logn)
--     2. Compute product: f_c = f_a · f_c    .. O(n)
--     3. Interpolate c: c = fft^{-1}(f_c)    .. O(n·logn)
mult :: [Double] -> [Double] -> [Double]
mult [] _ = []
mult a  b = map realPart c
 where
  n   = length a
  w   = exp (pi * (0 :+ 1) / fromIntegral n)
  f_a = fft (map (:+ 0) $ a ++ replicate n 0.0) w
  f_b = fft (map (:+ 0) $ b ++ replicate n 0.0) w
  f_c = zipWith (*) f_a f_b
  c   = map (/ (fromIntegral $ 2 * n)) $ fft f_c (1 / w)

round2 f = (fromInteger $ round $ f * 100) / 100
