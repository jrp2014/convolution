module Naive where

import Control.Parallel.Strategies

import qualified Data.Array as A
-- from https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
--
import Data.List (foldl', tails)
import qualified Data.Map as M
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

convolveA ::
     (A.Ix a, Integral a, Num b) => A.Array a b -> A.Array a b -> A.Array a b
convolveA x1 x2 =
  A.listArray
    (0, m3)
    [ sum [x1 A.! k * x2 A.! (n - k) | k <- [max 0 (n - m2) .. min n m1]]
    | n <- [0 .. m3]
    ]
        -- TODO::check whether bouds are correct
  where
    m1 = snd $ A.bounds x1
    m2 = snd $ A.bounds x2
    m3 = m1 + m2

convolveR :: (Num a) => [a] -> [a] -> [a]
convolveR xs ys = map sum $ foldr f [] xs
  where
    f x zs = foldr (g x) id ys ([] : zs)
    g x y a (z:zs) = ((x * y) : z) : a zs
    g x y a [] = [x * y] : a []

-- TODO:: refactor
convolveL :: (Num a) => [a] -> [a] -> [a]
convolveL xs ys = map sum $ foldl' (\h b x -> h (f b x)) id xs []
  where
    f x zs = foldl' (\h b y -> h (g x b y)) id ys id ([] : zs)
    g x y a (z:zs) = ((x * y) : z) : a zs
    g x y a [] = [x * y] : a []

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
  | DirectR
  | DirectL
  | VectorNaive
  | ArrayNaive
  deriving (Eq, Ord)

convTypes :: M.Map ConvType ([Int] -> [Int] -> [Int])
convTypes =
  M.fromList
    [ (Naive, convolve)
    , (Reduced, convolve')
    , (Parallel, parConvolve)
    , (DirectR, convolveR)
    , (DirectL, convolveL)
    ]

convVTypes :: M.Map ConvType (V.Vector Int -> V.Vector Int -> V.Vector Int)
convVTypes = M.fromList [(VectorNaive, convolveV)]

convATypes ::
     M.Map ConvType (A.Array Int Int -> A.Array Int Int -> A.Array Int Int)
convATypes = M.fromList [(ArrayNaive, convolveA)]
