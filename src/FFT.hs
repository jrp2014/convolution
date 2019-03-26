module FFT
  ( convolve
  , convolve'
  )
where

import           Data.Complex

-- https://www.lopezferrando.com/learning-haskell/
-- length fft is a power of 2
fft :: (RealFloat a) => [Complex a] -> Complex a -> [Complex a]
fft a w | pow2 (length a) = fft' a w
        | otherwise       = error "fft only defined for powers of 2"
 where
  pow2 :: Int -> Bool
  pow2 n | n == 1 || n == 2 = True
         | otherwise        = n `mod` 2 == 0 && pow2 (n `div` 2)

fft' :: (RealFloat a) => [Complex a] -> Complex a -> [Complex a]
fft' [a] _ = [a]
fft' a   w = zipWith (+) f_even (zipWith (*) ws1 f_odd)
  ++ zipWith (+) f_even (zipWith (*) ws2 f_odd)
 where
  -- Take even and odd coefficients of a(x)
  n               = length a
  (a_even, a_odd) = split a
  f_even          = fft' a_even (w * w)
  f_odd           = fft' a_odd (w * w)
  ws1             = take (n `div` 2) (iterate (* w) 1)
  ws2             = map negate ws1

-- Compute FFT of a_even(x) and a_odd(x) recursively
-- Compute 1st and 2nd half of unity roots (ws2 = - ws1)
split :: [a] -> ([a], [a])
split = foldr f ([], []) where f a (r1, r2) = (a : r2, r1)

-- Multiplication of polynomials: c(x) = a(x) · b(x)
--     * a(x) and b(x) have degree n (power of 2), c(x) has degree 2n
--   Algorithm:
--     1. Compute f_a = fft(a), f_b = fft(b)  .. O(n·logn)
--     2. Compute product: f_c = f_a · f_c    .. O(n)
--     3. Interpolate c: c = fft^{-1}(f_c)    .. O(n·logn)
convolve :: (RealFloat a) => [a] -> [a] -> [a]
convolve [] _ = []
convolve a  b = map realPart c
 where
  la        = length a
  lb        = length b
  n         = max la lb
  lab       = 2 * n
  w         = exp (pi * (0 :+ 1) / fromIntegral n)
  f_a       = fft (map (:+ 0) $ a ++ replicate (lab - la) 0) w
  f_b       = fft (map (:+ 0) $ b ++ replicate (lab - lb) 0) w
  normalize = 1.0 / (2.0 * fromIntegral n)
  f_c       = zipWith (\x y -> normalize * x * y) f_a f_b
  c         = init $ fft f_c (1 / w)

round2 :: (RealFrac a1, Fractional a2) => a1 -> a2
round2 f = fromInteger (round $ f * 100) / 100

-- http://www.robinscheibler.org/2013/02/13/real-fft.html
-- http://sar.kangwon.ac.kr/gisg/FFT_book.pdf (p189ff)
-- Uses 2, instead of 3 ffts
convolve' :: (RealFloat a) => [a] -> [a] -> [a]
convolve' xs ys = map realPart c
 where
  lxs       = length xs
  lys       = length ys
  n         = max lxs lys
  xs'       = xs ++ replicate (n - lxs) 0
  ys'       = ys ++ replicate (n - lys) 0
  zs        = zipWith (:+) xs' ys ++ replicate n 0
  w         = exp (pi * (0 :+ 1) / fromIntegral n)
  zs'       = fft zs w -- Z [n]
  zs''      = zs' ++ [head zs']
  zs'''     = reverse zs'' -- Z* [N - k]
  normalize = (0 :+ 1.0) / (8.0 * fromIntegral n)
-- g and h have complex conjugate (Hermitian) symmetry, so really only need
-- 0..n/2 of these
  f_c       = take (n + 1) $ zipWith
    (\x y -> let ystar = conjugate y in (x + ystar) * (ystar - x) * normalize)
    zs''
    zs'''
  f_c' = f_c ++ (map conjugate . tail . reverse $ tail f_c)
  c    = init $ fft f_c' (1 / w) -- drop the last 0

pow2 :: Int -> Bool
pow2 n | n == 1 || n == 2 = True
       | otherwise        = n `mod` 2 == 0 && pow2 (n `div` 2)
