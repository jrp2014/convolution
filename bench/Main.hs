module Main where

-- from https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
import           Criterion.Main
import qualified Data.Array                    as A
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as U
import           Naive

main :: IO ()
main = defaultMain
  [ bench "Naive Convolution"                (runConv Naive)
  , bench "Reduced Convolution"              (runConv Reduced)
  , bench "Parallelized Convolution"         (runConv Parallel)
  , bench "Vector Naive Convolution"         (runConvV VectorNaive)
  , bench "Unboxed Vector Naive Convolution" (runConvU UnboxedVectorNaive)
  , bench "Array Naive Convolution"          (runConvA ArrayNaive)
  , bench "Stream Naive Convolution"         (runConv StreamNaive)
  , bench "Direct Convolution (foldr)"       (runConv DirectR)
  , bench "Direct Convolution (foldl')"      (runConv DirectL)
  ]
 where
  runConv ctype =
    let hs     = [1 .. 1000]
        ts     = [1 .. 10000]
        convfn = fromJust $ M.lookup ctype convTypes
    in  nf (convfn hs) ts

  runConvV ctype =
    let hs     = V.enumFromN 1 1000
        ts     = V.enumFromN 1 10000
        convfn = fromJust $ M.lookup ctype convVTypes
    in  nf (convfn hs) ts

  runConvU ctype =
    let hs     = U.enumFromN 1 1000
        ts     = U.enumFromN 1 10000
        convfn = fromJust $ M.lookup ctype convUTypes
    in  nf (convfn hs) ts

  runConvA ctype =
    let hs     = A.listArray (0, 999) [1 .. 1000]
        ts     = A.listArray (0, 9999) [1 .. 10000]
        convfn = fromJust $ M.lookup ctype convATypes
    in  nf (convfn hs) ts
