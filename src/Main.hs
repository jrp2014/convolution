module Main where

import Naive

import Criterion.Main

-- from https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
--
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Array as A

main :: IO ()
main =
  defaultMain
    [ bench "Naive Convolution" (runConv Naive)
    , bench "Reduced Convolution" (runConv Reduced)
    , bench "Parallelized Convolution" (runConv Parallel)
    , bench "Vector Naive Convolution" (runConvV VectorNaive)
    , bench "Array Naive Convolution" (runConvA ArrayNaive)
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
    runConvA ctype =
      let hs = A.listArray (0,999) [1..1000]
          ts = A.listArray (0,9999) [ 1..10000 ]
          convfn = fromJust $ M.lookup ctype convATypes
       in nf (convfn hs) ts

