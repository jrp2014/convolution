
module Core12a where

-- http://wotug.org/papers/CPA-2012/Cole12a/Cole12a.pdf


convolve :: [Float] -> [Float] -> [Float]
convolve filter input = sum convolvedParts
  where
    oneFilterElement shift mul = (input .<. shift) ^∗ mul
    -- Get the RADIUS of a filter.
    r = length filter `div` 2
    convolvedParts = zipWith (oneFilterElement input) [(−r)..] filter
