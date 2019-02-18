module Naive where

-- from https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
import Control.Parallel.Strategies
import qualified Data.Array as A
import Data.List (foldl', tails)
import qualified Data.Map as M
import qualified Data.Vector as V

-- this seems to be a bit faster than regular sum in some cases
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl' (+) 0

convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate (length hs - 1) 0
      ts = pad ++ xs
   in roll (reverse hs) ts --  args wrong way around in the original
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
   in roll (V.reverse hs) ts
  where
    roll :: (Num a) => V.Vector a -> V.Vector a -> V.Vector a
    roll hs ts
      | V.null ts = V.empty
      | otherwise =
        let sample = V.sum $ V.zipWith (*) ts hs
         in V.cons sample (roll hs (V.tail ts))

convolveA ::
     (A.Ix a, Integral a, Num b) => A.Array a b -> A.Array a b -> A.Array a b
convolveA x1 x2 =
  A.listArray
    (0, m3)
    [ sum [x1 A.! k * x2 A.! (n - k) | k <- [max 0 (n - m2) .. min n m1]]
    | n <- [0 .. m3]
    ]
  where
    m1 = snd $ A.bounds x1
    m2 = snd $ A.bounds x2
    m3 = m1 + m2

convolveR :: (Num a) => [a] -> [a] -> [a]
convolveR xs ys = map sum' $ foldr f [] xs
  where
    f x zs = foldr (g x) id ys ([] : zs)
    g x y a (z:zs) = ((x * y) : z) : a zs
    g x y a [] = [x * y] : a []

-- TODO:: refactor
convolveL :: (Num a) => [a] -> [a] -> [a]
convolveL xs ys = map sum' $ foldl' (\h b x -> h (f b x)) id xs []
  where
    f x zs = foldl' (\h b y -> h (g x b y)) id ys id ([] : zs)
    g x y a (z:zs) = ((x * y) : z) : a zs
    g x y a [] = [x * y] : a []

convolve' :: (Num a) => [a] -> [a] -> [a]
convolve' hs xs =
  let pad = replicate (length hs - 1) 0
      ts = pad ++ xs
   in map (sum' . zipWith (*) (reverse hs)) (init $ tails ts)

parConvolve :: (NFData a, Num a) => [a] -> [a] -> [a]
parConvolve [] _ = []
parConvolve hs xs =
  let pad = replicate (length hs - 1) 0
      ts = pad ++ xs
   in parMap rdeepseq (sum' . zipWith (*) (reverse hs)) (init $ tails ts)

-- s and t must be of the same length
-- convolve' [1,2,3,4,0,0,0,0] [1,2,3,4,5,0,0,0] == [1,4,10,20,30,34,31,20]
-- works with infinite lists; needs to be padded for finite ones
convolveS :: Num a => [a] -> [a] -> [a]
convolveS s t = convolveS' (s ++ replicate (ll - ls) 0) (t ++ replicate (ll - lt) 0)
  where
    ls = length s
    lt = length t
    ll = ls + lt - 1
    convolveS' (hs:ts) t'@(ht:tt) =
      hs * ht : zipWith (+) (map (hs *) tt) (convolveS' ts t')

data ConvType
  = Naive
  | Reduced
  | Parallel
  | DirectR
  | DirectL
  | VectorNaive
  | ArrayNaive
  | StreamNaive
  deriving (Eq, Ord)

convTypes :: M.Map ConvType ([Int] -> [Int] -> [Int])
convTypes =
  M.fromList
    [ (Naive, convolve)
    , (Reduced, convolve')
    , (Parallel, parConvolve)
    , (DirectR, convolveR)
    , (DirectL, convolveL)
    , (StreamNaive, convolveS)
    ]

convVTypes :: M.Map ConvType (V.Vector Int -> V.Vector Int -> V.Vector Int)
convVTypes = M.fromList [(VectorNaive, convolveV)]

convATypes ::
     M.Map ConvType (A.Array Int Int -> A.Array Int Int -> A.Array Int Int)
convATypes = M.fromList [(ArrayNaive, convolveA)]
