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
    f_even = fft a_even (w * w)
    f_odd = fft a_odd (w * w)
    ws1 = take (length a `div` 2) (iterate (* w) 1)
    ws2 = map negate ws1

-- Compute FFT of a_even(x) and a_odd(x) recursively
-- Compute 1st and 2nd half of unity roots (ws2 = - ws1)
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
    c = init $ map (/ (fromIntegral $ 2 * n)) $ fft f_c (1 / w)

round2 :: (RealFrac a1, Fractional a2) => a1 -> a2
round2 f = fromInteger (round $ f * 100) / 100

-- http://www.robinscheibler.org/2013/02/13/real-fft.html
--convolve' :: (RealFloat a) => [a] -> [a] -> [a]
-- http://sar.kangwon.ac.kr/gisg/FFT_book.pdf (p189ff)
-- Uses 2, instead of 3 ffts
convolve' :: (RealFloat a) => [a] -> [a] -> [a]
convolve' xs ys = map realPart c
  where
    n = length xs -- == length ys, even
    zs = zipWith (:+) xs ys ++ replicate n 0 --  ++ replicate n (0 :+0)
    w = exp (pi * (0 :+ 1) / fromIntegral n)
    zs' = fft zs w -- Z [n]
    zs'' = zs' ++ [head zs']
    zs''' = reverse zs'' -- Z* [N - k]
  -- g and h have complex conjugate (Hermitian) symmetry, so need onlys
  -- 0..n/2
    g = init $
      zipWith
        (\x y -> 0.5 * ((realPart x + realPart y) :+ (imagPart x - imagPart y)))
        zs''
        zs'''
    h = init $
      zipWith
        (\x y -> 0.5 * ((imagPart x + imagPart y) :+ (realPart y - realPart x)))
        zs''
        zs'''
    f_c = zipWith (*) g h
    c = init $ map (/ (fromIntegral $ 2 * n)) $ fft f_c (1 / w)

convolve'' :: (Num a) => [a] -> [a] -> [a]
convolve'' hs xs =
  let pad = replicate (length hs - 1) 0
      ts = pad ++ xs
   in roll (reverse hs) ts --  args wrong way around in the original
  where
    roll :: (Num a) => [a] -> [a] -> [a]
    roll _ [] = []
    roll hs ts =
      let sample = sum $ zipWith (*) ts hs
       in sample : roll hs (tail ts)
