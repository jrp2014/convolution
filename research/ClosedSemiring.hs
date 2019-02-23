module SemiRing where

-- import Data.Semiring

infixl 9 @.

infixl 8 @+

class ClosedSemiring r where
  zero, one :: r
  closure :: r -> r
  (@+), (@.) :: r -> r -> r

instance ClosedSemiring Bool where
  zero = False
  one  = True
  closure _ = True
  (@+) = (||)
  (@.) = (&&)

instance ClosedSemiring r => ClosedSemiring [r] where
  zero = []
  one  = [one]

  closure []      = one
  closure (a : p) = r where r = [closure a] @. (one : (p @. r))

  []       @+ y        = y
  x        @+ []       = x
  (x : xs) @+ (y : ys) = (x @+ y) : (xs @+ ys)

  [] @. _  = []
  _  @. [] = []
  (a : p) @. (b : q) =
    (a @. b) : (map (a @.) q @+ map (@. b) p @+ (zero : (p @. q)))
