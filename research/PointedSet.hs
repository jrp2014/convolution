module PointedSet where

-- http://comonad.com/reader/2008/the-pointed-set-comonad/

import           Control.Comonad
import           Data.List       (inits, tails)

data PointedSet a =
  PointedSet a [a]
  deriving (Eq, Ord, Show, Read)

instance Functor PointedSet where
  fmap f (PointedSet x xs) = PointedSet (f x) $ fmap f xs

instance Comonad PointedSet where
  extract (PointedSet x _) = x

--  duplicate xxs@(PointedSet x xs) = PointedSet xxs $ refocus [] x xs
--   where
--    refocus :: [a] -> a -> [a] -> [PointedSet a]
--    refocus acc x (y : ys) =
--      PointedSet y (acc ++ (x : ys)) : refocus (y : acc) x ys
--    refocus _ _ [] = []

  duplicate xxs@(PointedSet x xs) =
    PointedSet xxs . fmap listToSet . tail $ rotations (x : xs)
   where
    rotations :: [a] -> [[a]]
    rotations xs = init $ zipWith (++) (tails xs) (inits xs)
    listToSet (x : xs) = PointedSet x xs


smooth :: Fractional a => a -> PointedSet a -> a
smooth w (PointedSet a as) =
  w * a + (1 - w) * sum as / fromIntegral (length as)



