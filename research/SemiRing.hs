{-# LANGUAGE FlexibleInstances #-}


infixl 9 @.
infixl 8 @+
class Semiring r where
  zero, one :: r
  closure :: r -> r
  (@+), (@.) :: r -> r -> r


instance Semiring r => Semiring [r] where
  zero = []
  one = [one]

  [] @+ y = y
  x @+ [] = x
  (x:xs) @+ (y:ys) = (x @+ y):(xs @+ ys)

  [] @. _ = []
  _ @. [] = []
  (a:p) @. (b:q) = (a @. b):(map (a @.) q @+ map (@. b) p @+ (zero:(p @. q)))

  closure [] = one
  closure (a:p) = r
    where r = [closure a] @. (one:(p @. r))


instance Num a => Semiring a where
  zero = 0
  one = 1
  (@+) = (+)
  (@.) = (*)
