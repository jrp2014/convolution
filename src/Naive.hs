{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Naive
  ( convolve
  , convolve'
  , convolveV
  , convolveUV
  , convolveA
  , convolveUA
  , convolveR
  , convolveL
  , parConvolve
  , convolveS
  , (#)
  , ConvType (..)
  , convTypes
  , convVTypes
  , convUTypes
  , convATypes
  , convUATypes
  )
where

-- from https://www.blaenkdenum.com/posts/naive-convolution-in-haskell/
import           Control.Parallel.Strategies
import qualified Data.Array                    as A
import qualified Data.Array.Unboxed            as UA

import           Data.List                      ( foldl'
                                                , tails
                                                )
import qualified Data.Map                      as M
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV

-- this seems to be a bit faster than regular sum in some cases
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl' (+) 0

-- | Zips together two lists using a function,
-- and evaluates the result list in parallel.
-- -- [V slow)
parZipWith :: Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith strat z as bs = zipWith z as bs `using` parList strat

dotp :: Num a => [a] -> [a] -> a
dotp = go 0
 where
  go !acc (x : xs) (y : ys) = go (acc + x * y) xs ys
  go !acc _        _        = acc

dotp' :: Num p => V.Vector p -> V.Vector p -> p
dotp' u v = loop 0 0 0
 where
  m = V.length u
  n = V.length v
  loop z i j | i < n && j < m = loop (z + u V.! i * v V.! j) (i + 1) (j + 1)
             | otherwise      = z

dotProduct :: Num a => [a] -> [a] -> a
dotProduct = (sum' .) . zipWith (*)

convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate (length hs - 1) 0
      ts  = pad ++ xs
  in  roll (reverse hs) ts --  args wrong way around in the original
 where
  roll :: (Num a) => [a] -> [a] -> [a]
  roll _ [] = []
  roll hs ts =
    let sample = sum' $ zipWith (*) ts hs
  --let sample = dotp ts hs
                                          in sample : roll hs (tail ts)
{-# INLINABLE convolve #-}

convolve' :: (Num a) => [a] -> [a] -> [a]
convolve' hs xs =
  let pad = replicate (length hs - 1) 0
      ts  = pad ++ xs
  in  map (sum' . zipWith (*) (reverse hs)) (init $ tails ts)
{-# INLINABLE convolve' #-}

convolveV :: (Num a) => V.Vector a -> V.Vector a -> V.Vector a
convolveV hs xs =
  let pad = V.replicate (V.length hs - 1) 0
      ts  = (V.++) pad xs
  in  roll (V.reverse hs) ts
 where
  roll :: (Num a) => V.Vector a -> V.Vector a -> V.Vector a
  roll hs ts
    | V.null ts
    = V.empty
    | otherwise
    --let sample = dotp ts hs -- is no faster then the  clearer version
    = let sample = V.sum $ V.zipWith (*) ts hs
      in  V.cons sample (roll hs (V.tail ts))
{-# INLINABLE convolveV #-}

convolveUV :: (Num a, UV.Unbox a) => UV.Vector a -> UV.Vector a -> UV.Vector a
convolveUV hs xs =
  let pad = UV.replicate (UV.length hs - 1) 0
      ts  = (UV.++) pad xs
  in  roll (UV.reverse hs) ts
 where
  roll :: (Num a, UV.Unbox a) => UV.Vector a -> UV.Vector a -> UV.Vector a
  roll hs ts
    | UV.null ts
    = UV.empty
    | otherwise
    --let sample = dotp ts hs -- is no faster then the  clearer version
    = let sample = UV.sum $ UV.zipWith (*) ts hs
      in  UV.cons sample (roll hs (UV.tail ts))
{-# INLINABLE convolveUV #-}

convolveA
  :: (A.Ix a, Integral a, Num b) => A.Array a b -> A.Array a b -> A.Array a b
convolveA x1 x2 = A.listArray
  (0, m3)
  [ sum [ x1 A.! k * x2 A.! (n - k) | k <- [max 0 (n - m2) .. min n m1] ]
  | n <- [0 .. m3]
  ]
 where
  m1 = snd $ A.bounds x1
  m2 = snd $ A.bounds x2
  m3 = m1 + m2
{-# INLINABLE convolveA #-}

convolveUA
  :: (UA.Ix a, Integral a, Num b, UA.IArray UA.UArray b)
  => UA.UArray a b
  -> UA.UArray a b
  -> UA.UArray a b
convolveUA x1 x2 = UA.listArray
  (0, m3)
  [ sum [ x1 UA.! k * x2 UA.! (n - k) | k <- [max 0 (n - m2) .. min n m1] ]
  | n <- [0 .. m3]
  ]
 where
  m1 = snd $ UA.bounds x1
  m2 = snd $ UA.bounds x2
  m3 = m1 + m2
{-# INLINABLE convolveUA #-}

convolveR :: (Num a) => [a] -> [a] -> [a]
convolveR xs ys = foldr f [] xs
 where
  f x zs = foldr (g x) id ys (0 : zs)
  --g x y a (z:zs) = ((x * y) : z) : a zs
  --g x y a [] = [x * y] : a []
  g x y a []       = x * y : a []
  g x y a (z : zs) = x * y + z : a zs
{-# INLINABLE convolveR #-}

-- TODO:: refactor
convolveL :: (Num a) => [a] -> [a] -> [a]
convolveL xs ys = foldl' (\h b x -> h (f b x)) id xs []
 where
  f x zs = foldl' (\h b y -> h (g x b y)) id ys id (0 : zs)
  g x y a (z : zs) = (x * y + z) : a zs
  g x y a []       = x * y : a []
{-# INLINABLE convolveL #-}

parConvolve :: (NFData a, Num a) => [a] -> [a] -> [a]
parConvolve [] _ = []
parConvolve hs xs =
  let pad = replicate (length hs - 1) 0
      ts  = pad ++ xs
  in  parMap rdeepseq (sum' . zipWith (*) (reverse hs)) (init $ tails ts)
{-# INLINABLE parConvolve #-}

-- s and t must be of the same length
-- convolve' [1,2,3,4,0,0,0,0] [1,2,3,4,5,0,0,0] == [1,4,10,20,30,34,31,20]
-- works with infinite lists; needs to be padded for finite ones
convolveS :: Num a => [a] -> [a] -> [a]
convolveS s t = convolveS' (s ++ replicate (ll - ls) 0)
                           (t ++ replicate (ll - lt) 0)
 where
  ls = length s
  lt = length t
  ll = ls + lt - 1
  convolveS' (hs : ts) t'@(ht : tt) =
    hs * ht : zipWith (+) (map (hs *) tt) (convolveS' ts t')
{-# INLINABLE convolveS #-}


(#) :: (Num a, NFData a) => [a] -> [a] -> [a]
(a : b) # c = zipWith (+) (0 : b # c) $ map (a *) c ++ [] # b
_       # c = 0 <$ c
{-# INLINABLE (#) #-}

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
  deriving (Eq, Ord)

convTypes :: M.Map ConvType ([Int] -> [Int] -> [Int])
convTypes = M.fromList
  [ (Naive      , convolve)
  , (Reduced    , convolve')
  , (Parallel   , parConvolve)
  , (DirectR    , convolveR)
  , (DirectL    , convolveL)
  , (StreamNaive, convolveS)
  , (Golf       , (#))
  ]

convVTypes :: M.Map ConvType (V.Vector Int -> V.Vector Int -> V.Vector Int)
convVTypes = M.fromList [(VectorNaive, convolveV)]

convUTypes :: M.Map ConvType (UV.Vector Int -> UV.Vector Int -> UV.Vector Int)
convUTypes = M.fromList [(UnboxedVectorNaive, convolveUV)]

convATypes
  :: M.Map ConvType (A.Array Int Int -> A.Array Int Int -> A.Array Int Int)
convATypes = M.fromList [(ArrayNaive, convolveA)]

convUATypes
  :: M.Map
       ConvType
       (UA.UArray Int Int -> UA.UArray Int Int -> UA.UArray Int Int)
convUATypes = M.fromList [(UnboxedArrayNaive, convolveUA)]
