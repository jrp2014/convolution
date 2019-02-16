{-# LANGUAGE DeriveFunctor #-}

module CArray where

import           Data.Array
import           Control.Comonad

data CArray i a = CA (Array i a) i deriving (Functor, Show)

fromList :: [a] -> CArray Int a
fromList a = CA (listArray (0, length a - 1) a) 0

instance Ix i => Comonad (CArray i) where
  extract (CA a i) = a ! i
  extend f (CA a i) =
    let es' = map (\j -> (j, f (CA a j))) (indices a)
    in  CA (array (bounds a) es') i


(?) :: (Ix i, Num a, Num i) => CArray i a -> i -> a
(CA a i) ? i' = if (inRange (bounds a) (i + i')) then a ! (i + i') else 0

conv :: CArray Int Int -> Int
conv a = a ? 0 + 2 * a ? 1 + 3 * a ? 2 + 4 * a ? 3 + 5 * a ? 4
