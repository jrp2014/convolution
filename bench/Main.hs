module Main where

-- from https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
import           Criterion.Main
import qualified Data.Array                    as A
import qualified Data.Array.Unboxed            as UA
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
import           Naive
import qualified FFT
import           Semi

data ConvType
  = Naive
  | Reduced
  | Parallel
  | DirectR
  | DirectL
  | VectorNaive
  | UnboxedVectorNaive
  | ArrayNaive
  | UnboxedArrayNaive
  | StreamNaive
  | Golf
  | Conal
  | FFT1
  | FFT2
  deriving (Eq, Ord)

type BaseReal = Double

convTypes :: M.Map ConvType ([BaseReal] -> [BaseReal] -> [BaseReal])
convTypes = M.fromList
  [ (Naive      , convolve)
  , (Reduced    , convolve')
  , (Parallel   , parConvolve)
  , (DirectR    , convolveR)
  , (DirectL    , convolveL)
  , (StreamNaive, convolveS)
  , (Golf       , (#))
  , (Conal      , (<.>))
  , (FFT1       , FFT.convolve)
  , (FFT2       , FFT.convolve')
  ]

convVTypes
  :: M.Map
       ConvType
       (V.Vector BaseReal -> V.Vector BaseReal -> V.Vector BaseReal)
convVTypes = M.fromList [(VectorNaive, convolveV)]

convUTypes
  :: M.Map
       ConvType
       (UV.Vector BaseReal -> UV.Vector BaseReal -> UV.Vector BaseReal)
convUTypes = M.fromList [(UnboxedVectorNaive, convolveUV)]

convATypes
  :: M.Map
       ConvType
       (A.Array Int BaseReal -> A.Array Int BaseReal -> A.Array Int BaseReal)
convATypes = M.fromList [(ArrayNaive, convolveA)]

convUATypes
  :: M.Map
       ConvType
       (  UA.UArray Int BaseReal
       -> UA.UArray Int BaseReal
       -> UA.UArray Int BaseReal
       )
convUATypes = M.fromList [(UnboxedArrayNaive, convolveUA)]


main :: IO ()
main = defaultMain
  [ bgroup
    "FFT"
    [ bench "Full complex"      (runConv FFT1)
    , bench "Simultaneous real" (runConv FFT2)
    ]
  , bgroup
    "Naive"
    [ bench "Naive Convolution"                (runConv Naive)
    , bench "Golf"                             (runConv Golf)
    , bench "Conal"                            (runConv Conal)
    , bench "Reduced Convolution"              (runConv Reduced)
    , bench "Parallelized Convolution"         (runConv Parallel)
    , bench "Vector Naive Convolution"         (runConvV VectorNaive)
    , bench "Unboxed Vector Naive Convolution" (runConvUV UnboxedVectorNaive)
    , bench "Array Naive Convolution"          (runConvA ArrayNaive)
    , bench "Unboxed Array Naive Convolution"  (runConvUA UnboxedArrayNaive)
    , bench "Stream Naive Convolution"         (runConv StreamNaive)
    , bench "Direct Convolution (foldr)"       (runConv DirectR)
    , bench "Direct Convolution (foldl')"      (runConv DirectL)
    ]
  ]
 where
  runConv ctype =
    let hs     = [1 .. 1024]
        ts     = [1 .. 8192]
        convfn = fromJust $ M.lookup ctype convTypes
    in  nf (convfn hs) ts

  runConvV ctype =
    let hs     = V.enumFromN 1 1024
        ts     = V.enumFromN 1 8192
        convfn = fromJust $ M.lookup ctype convVTypes
    in  nf (convfn hs) ts

  runConvUV ctype =
    let hs     = UV.enumFromN 1 1024
        ts     = UV.enumFromN 1 8192
        convfn = fromJust $ M.lookup ctype convUTypes
    in  nf (convfn hs) ts

  runConvA ctype =
    let hs     = A.listArray (0, 1023) [1 .. 1024]
        ts     = A.listArray (0, 8191) [1 .. 8192]
        convfn = fromJust $ M.lookup ctype convATypes
    in  nf (convfn hs) ts

  runConvUA ctype =
    let hs     = UA.listArray (0, 1023) [1 .. 1024]
        ts     = UA.listArray (0, 8191) [1 .. 8192]
        convfn = fromJust $ M.lookup ctype convUATypes
    in  whnf (convfn hs) ts
