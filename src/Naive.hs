module Main where

import Control.Parallel.Strategies
import Criterion.Main

-- from https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
--
import Data.List (foldl', tails)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Vector as V

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl' (+) 0

convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate (length hs - 1) 0
      ts = pad ++ xs
   in roll ts (reverse hs)
  where
    roll :: (Num a) => [a] -> [a] -> [a]
    roll _ [] = []
    roll hs ts =
      let sample = sum' $ zipWith (*) ts hs
       in sample : roll hs (tail ts)

convolveV :: (Num a) => V.Vector a -> V.Vector a -> V.Vector a
convolveV hs xs =
  let pad = V.replicate (V.length hs - 1) 0
      ts = (V.++) pad xs
   in roll ts (V.reverse hs)
  where
    roll :: (Num a) => V.Vector a -> V.Vector a -> V.Vector a
    roll hs ts
      | V.null ts = V.empty
      | otherwise =
        let sample = V.sum $ V.zipWith (*) ts hs
         in V.cons sample (roll hs (V.tail ts))

convolve' :: (Num a) => [a] -> [a] -> [a]
convolve' hs xs =
  let pad = replicate (length hs - 1) 0
      ts = pad ++ xs
   in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)

parConvolve :: (NFData a, Num a) => [a] -> [a] -> [a]
parConvolve hs xs =
  let pad = replicate (length hs - 1) 0
      ts = pad ++ xs
   in parMap rdeepseq (sum . zipWith (*) (reverse hs)) (init $ tails ts)

data ConvType
  = Naive
  | Reduced
  | Parallel
  | VectorNaive
  deriving (Eq, Ord)

convTypes :: M.Map ConvType ([Int] -> [Int] -> [Int])
convTypes =
  M.fromList [(Naive, convolve), (Reduced, convolve'), (Parallel, parConvolve)]

convVTypes :: M.Map ConvType (V.Vector Int -> V.Vector Int -> V.Vector Int)
convVTypes = M.fromList [(VectorNaive, convolveV)]

main :: IO ()
main =
  defaultMain
    [ bench "Naive Convolution" (runConv Naive)
    , bench "Reduced Convolution" (runConv Reduced)
    , bench "Parallelized Convolution" (runConv Parallel)
    , bench "Vector Naive Convolution" (runConvV VectorNaive)
    ]
  where
    runConv ctype =
      let hs = [1 .. 1000 :: Int]
          ts = [1 .. 10000 :: Int]
          convfn = fromJust $ M.lookup ctype convTypes
       in nf (convfn hs) ts
    runConvV ctype =
      let hs = V.enumFromN 1 1000
          ts = V.enumFromN 1 10000
          convfn = fromJust $ M.lookup ctype convVTypes
       in nf (convfn hs) ts
