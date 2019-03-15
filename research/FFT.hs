module FFT where

import Data.Complex

-- https://www.lopezferrando.com/learning-haskell/
fft :: (RealFloat a) => [Complex a] -> Complex a -> [Complex a]
fft [a] _ = [a]
fft a w =
  zipWith (+) f_even (zipWith (*) ws1 f_odd) ++
  zipWith (+) f_even (zipWith (*) ws2 f_odd)
  -- Take even and odd coefficients of a(x)
  where
    (a_even, a_odd) = split a
  -- Compute FFT of a_even(x) and a_odd(x) recursively
    f_even = fft a_even (w * w)
    f_odd = fft a_odd (w * w)
  -- Compute 1st and 2nd half of unity roots (ws2 = - ws1)
    ws1 = take (length a `div` 2) (iterate (* w) 1)
    ws2 = map negate ws1

split :: [a] -> ([a], [a])
split = foldr f ([], [])
  where
    f a (r1, r2) = (a : r2, r1)

-- Multiplication of polynomials: c(x) = a(x) · b(x)
--     * a(x) and b(x) have degree n (power of 2), c(x) has degree 2n
--   Algorithm:
--     1. Compute f_a = fft(a), f_b = fft(b)  .. O(n·logn)
--     2. Compute product: f_c = f_a · f_c    .. O(n)
--     3. Interpolate c: c = fft^{-1}(f_c)    .. O(n·logn)
convolve :: (RealFloat a) => [a] -> [a] -> [a]
convolve [] _ = []
convolve a b = map realPart c
  where
    n = length a
    padding = replicate n 0
    w = exp (pi * (0 :+ 1) / fromIntegral n)
    f_a = fft (map (:+ 0) $ a ++ padding) w
    f_b = fft (map (:+ 0) $ b ++ padding) w
    f_c = zipWith (*) f_a f_b
    c = map (/ (fromIntegral $ 2 * n)) $ fft f_c (1 / w)

round2 :: (RealFrac a1, Fractional a2) => a1 -> a2
round2 f = fromInteger (round $ f * 100) / 100
