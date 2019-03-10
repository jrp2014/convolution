module FFT where

import Data.Complex

-- Cooley-Tukey
fft :: RealFloat a => [Complex a] -> [Complex a]
fft [] = []
fft [x] = [x]
fft xs = zipWith (+) ys ts ++ zipWith (-) ys ts
  where
    n = length xs
    ys = fft evens
    zs = fft odds
    (evens, odds) = split xs
    split [] = ([], [])
    split [x] = ([x], [])
    split (x:y:xs) = (x : xt, y : yt)
      where
        (xt, yt) = split xs
    ts = zipWith (\z k -> exp' k n * z) zs [0 ..]
    exp' k n = cis $ -2 * pi * fromIntegral k / fromIntegral n

main :: IO ()
main = mapM_ print $ fft [1, 1, 1, 1, 0, 0, 0, 0]
