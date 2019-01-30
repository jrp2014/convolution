module Main where

import Control.Parallel.Strategies
import Criterion.Main
-- from https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
--
import Data.List (tails)
import qualified Data.Map as M
import Data.Maybe (fromJust)

convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate (length hs - 1) 0
      ts = pad ++ xs
   in roll ts (reverse hs)
  where
    roll :: (Num a) => [a] -> [a] -> [a]
    roll _ [] = []
    roll hs ts =
      let sample = sum $ zipWith (*) ts hs
       in sample : roll hs (tail ts)

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
  deriving (Eq, Ord)

convTypes :: M.Map ConvType ([Int] -> [Int] -> [Int])
convTypes =
  M.fromList [(Naive, convolve), (Reduced, convolve'), (Parallel, parConvolve)]

main :: IO ()
main =
  defaultMain
    [ bench "Naive Convolution" (runConv Naive)
    , bench "Reduced Convolution" (runConv Reduced)
    , bench "Parallelized Convolution" (runConv Parallel)
    ]
  where
    runConv ctype =
      let hs = [1 .. 1000 :: Int]
          ts = [1 .. 10000 :: Int]
          convfn = fromJust $ M.lookup ctype convTypes
       in nf (convfn hs) ts
