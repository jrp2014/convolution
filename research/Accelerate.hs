{-# LANGUAGE OverloadedLists #-}

module AccelerateConv where

import qualified Data.Array.Accelerate as A

conv ::
     A.Acc (A.Vector Int) -> A.Acc (A.Vector Int) -> A.Acc (A.Vector Int)
conv hs xs =
  let pad = A.replicate (A.length hs - 1) 0
      ts = pad A.++ xs
   in roll (A.reverse hs) ts
  where
    roll :: (A.Vector Int) -> (A.Vector Int) -> (A.Vector Int)
    roll hs ts
      | A.null ts = A.fromList (A.Z A.:. 0) [] :: A.Vector Int
      | otherwise =
        let sample = A.sum $ A.zipWith (*) ts hs
         in A.unit sample A.++ roll hs (A.tail ts)







x :: A.Vector Int
x = [1, 2, 3, 4, 5]

y :: A.Vector Int
y = [1, 2, 3, 4, 5]
