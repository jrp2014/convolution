{-# LANGUAGE BangPatterns #-}
module RepaConv where

import qualified Data.Array.Repa as R

type Complex = (Float, Float)
type Data = R.Array R.U R.DIM1 Complex
type Stencil = R.Array R.D R.DIM1 Float

complexPlus :: Complex -> Complex -> Complex
complexPlus (!r0, !i0) (!r1, !i1) = (r0 + r1, i0 + i1)

stencilSize :: Int
stencilSize = 8192

stencilShape :: R.DIM1
stencilShape = R.Z R.:. stencilSize :: R.DIM1

-- Assumes stencil size is a power of 2
genInput :: Int -> Data
genInput size = R.computeS $ R.fromFunction shape genOne
  where
    shape =  R.Z  R.:. size + stencilSize :: R.DIM1
    padLeft = stencilSize `div` 2
    padRight = stencilSize `div` 2
    indexLeft = padLeft - 1
    indexRight = size + padRight - 1
    genOne tag =
      let
      idx0 = R.toIndex shape tag
      idx
        | idx0 < indexLeft = indexLeft
        | idx0 > indexRight = indexRight
        | otherwise = idx0
      r    = fromIntegral ((2*idx) `mod` 7)
      i    = fromIntegral ((2*idx+1) `mod` 9)
      in (r, i)

checkSum :: Data -> Float
checkSum d = r + i
  where
    (r, i) = R.foldAllS complexPlus (0.0, 0.0) d

convolve0 :: Complex -> Float -> Complex
convolve0 (!r, !i) s = (r*s - i*s, r*s + i*s)

convolve :: Int -> Stencil -> Data -> Data
convolve !size stencil input = R.computeUnboxedS $ R.fromFunction shape genOne
  where
    genOne tag =
      let
        elements = R.extract tag stencilShape input
        partials = R.zipWith convolve0 elements stencil
        final = R.foldAllS complexPlus (0.0, 0.0) partials
      in final
    shape =  R.Z  R.:. size :: R.DIM1

run :: Int -> Stencil -> Int -> Data -> Data
run size stencil n input | n == 0    = input
                         | otherwise = run size stencil (n-1) $ convolve size stencil input

buildIt :: [String] -> (Stencil, Data)
buildIt args = 
  let input   = genInput size
      stencil = R.fromFunction stencilShape $ const 2
      runIt   = run size stencil iterations input
      --showIt = fmap (show . checkSum) runIt
  --return (runIt, showIt)
   in (stencil, runIt)
  where
    (size, iterations) = case args of
             []     -> (1000000::Int, 1::Int)
             [s]    -> (read s::Int, 1::Int)
             [s, i] -> (read s::Int, read i::Int)

