module Overlap where

window l m xs
  | length xs >= l = (take l xs ++ replicate (n - l) 0) : window l m (drop l xs)
  | otherwise = []
  where
    n = l + m - 1
