module Brent where

-- from https://byorgey.wordpress.com/2008/04/22/list-convolutions/

import Prelude hiding ((+),(*),(**))
import qualified Prelude as P

type List a = [a] -> [a]

fromList :: [a] -> List a
fromList = (++)

toList :: List a -> [a]
toList = ($[])

singleton :: a -> List a
singleton = (:)

empty :: List a
empty = id

(+) :: [b -> c] -> [a -> b] -> [a -> c]
(+) = zipWith (.)

(*) :: Num a => a -> a -> List a
x * y = singleton ((P.*) x y)

(.*) :: Num a => a -> [a] -> [List a]
x .* ys = map (x*) ys

(*.) :: Num a => [a] -> a -> [List a]
ys *. x = map (*x) ys

(**) :: Num a => [a] -> [a] -> [List a]
(x:xs) ** (y:ys) = x * y : (x .* ys) + (empty : (xs ** ys)) + (xs *. y)
_      ** _      = []


convolveB :: Num b => [b] -> [b] -> [b]
convolveB  xs ys = map (sum . toList) $ xs ** ys
