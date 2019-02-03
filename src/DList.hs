module DList where

-- from https://byorgey.wordpress.com/2008/04/22/list-convolutions/
import Prelude hiding ((*), (**), (+))
--import qualified Prelude as P

type List a = [a] -> [a]

fromList :: [a] -> List a
fromList = (++)

toList :: List a -> [a]
toList = ($ [])

singleton :: a -> List a
singleton = (:)

empty :: List a
empty = id

(+) :: [b -> c] -> [a -> b] -> [a -> c]
(+) = zipWith (.)

(*) :: a -> b -> List (a, b)
x * y = singleton (x, y)

(.*) :: a -> [b] -> [List (a, b)]
x .* ys = map (x *) ys

(*.) :: [a] -> b -> [List (a, b)]
ys *. x = map (* x) ys

(**) :: [a] -> [b] -> [List (a, b)]
[] ** _ = []
_ ** [] = []
(x:xs) ** (y:ys) = x * y : (x .* ys) + (empty : (xs ** ys)) + (xs *. y)

example :: [(Integer, Integer)]
example = concatMap toList $ [1 ..] ** [1 ..]
