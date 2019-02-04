module DiffList where

-- from https://byorgey.wordpress.com/2008/04/22/list-convolutions/
import Prelude hiding ((*), (**), (+))
import qualified Prelude as P

type List a = [a] -> [a]

fromList :: [a] -> List a
fromList = (++)

toList :: List a -> [a]
toList = ($ [])

toDotP :: Num a => List (a, a) -> a
toDotP = P.sum . P.map (uncurry (P.*)) . toList

singleton :: a -> List a
singleton = (:)

empty :: List a
empty = id

-- | /O(1)/. Append dlists
append :: List a -> List a -> List a
{-# INLINE append #-}
append xs ys = xs . ys

length :: List a -> Int
length = P.length . toList

-- | /O(1)/. Prepend a single element to a dlist
infixr `cons`
cons :: a -> List a -> List a
{-# INLINE cons #-}
cons x xs = (x :) . xs

-- | /O(1)/. Append a single element to a dlist
infixl `snoc`
snoc :: List a -> a -> List a
{-# INLINE snoc #-}
snoc xs x = xs . (x :)

replicate :: Int -> a -> List a
{-# INLINE replicate #-}
replicate n x xs = go n
  where
    go m
      | m <= 0 = xs
      | otherwise = x : go (m - 1)

reverse :: [a] -> [a]
reverse as = reverse' as []
  where
    reverse' :: [a] -> List a
    reverse' (a:as) = reverse' as `append` singleton a
    reverse' [] = empty

-- | /O(n)/. Foldr over difference lists
foldr :: (a -> b -> b) -> b -> List a -> b
{-# INLINE foldr #-}
foldr f b = P.foldr f b . toList

-- | /O(n)/. Map over difference lists.
map :: (a -> b) -> List a -> List b
map f = DiffList.foldr (cons . f) empty

{-# INLINE map #-}
(+) :: [b -> c] -> [a -> b] -> [a -> c]
(+) = zipWith (.)

(*) :: a -> b -> List (a, b)
x * y = singleton (x, y)

(.*) :: a -> [b] -> [List (a, b)]
x .* ys = P.map (x *) ys

(*.) :: [a] -> b -> [List (a, b)]
ys *. x = P.map (* x) ys

(**) :: [a] -> [b] -> [List (a, b)]
[] ** _ = []
_ ** [] = []
(x:xs) ** (y:ys) = x * y : (x .* ys) + (empty : (xs ** ys)) + (xs *. y)

example :: [(Integer, Integer)]
example = concatMap toList $ [1 ..] ** [1 ..]

eg2 = convolveD  [1, 2, 3, 4] [1, 2, 3, 4, 5]
eg3 = convolve  [1, 2, 3, 4] [1, 2, 3, 4, 5]

convolveD :: (Num a) => [a] -> [a] -> [a]
convolveD hs xs =
  let pad = P.replicate (P.length hs - 1) 0
      ts = xs ++ pad
   in P.map toDotP $ ts ** hs

convolve :: Num a => [a] -> [a] -> [a]
convolve xs ys = P.map sum $ P.foldr f [] xs
  where
    f x zs = P.foldr (g x) id ys ([] : zs)
    g x y a (z:zs) = ((x P.* y) : z) : a zs
    g x y a [] = [x P.* y] : a []

