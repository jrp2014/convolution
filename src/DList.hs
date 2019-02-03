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
append       :: List a -> List a -> List a
{-# INLINE append #-}
append xs ys = xs . ys

length :: List a -> Int
length = P.length . toList

-- | /O(1)/. Prepend a single element to a dlist
infixr `cons`
cons        :: a -> List a -> List a
cons x xs   =  (x:) . xs
{-# INLINE cons #-}

replicate :: Int -> a -> List a
{-# INLINE replicate #-}
replicate n x xs = go n
  where
    go m
      | m <= 0 = xs
      | otherwise = x : go (m - 1)

reverse :: List a -> List a
reverse = undefined

-- | /O(n)/. Foldr over difference lists
foldr        :: (a -> b -> b) -> b -> List a -> b
{-# INLINE foldr #-}
foldr f b    = P.foldr f b . toList

-- | /O(n)/. Map over difference lists.
map          :: (a -> b) -> List a -> List b
map f        = DiffList.foldr (cons . f) empty
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

(***) :: [a] -> [b] -> [List (a, b)]
[] *** _ = []
_ *** [] = []
(x:xs) *** (y:ys) = x * y : (x .* ys) + (empty : (xs ** ys)) + (xs *. y)

example :: [(Integer, Integer)]
example = concatMap toList $ [1 ..] ** [1 ..]

eg2 = P.map toDotP $ [1, 2, 3, 4, 0, 0, 0, 0] ** [1, 2, 3, 4, 5]

convolveD :: (Num a) => List a -> List a -> [a]
convolveD hs xs =
  let pad = undefined
   in undefined
