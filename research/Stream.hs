module Stream where

-- https://www.cs.ox.ac.uk/ralf.hinze/publications/CSC.pdf
--
import qualified Data.Stream                   as S
import           Data.Stream                    ( Stream(..)
                                                , (<:>)
                                                )

instance (Num a) => Num (Stream a) where
   (+)          =  S.zipWith (+)
   (-)          =  S.zipWith (-)
   (*)          =  S.zipWith (*)
   negate       =  S.map negate
   abs          =  S.map abs
   signum       =  S.map signum
   fromInteger  =  S.repeat . fromInteger

convolve :: Num a => Stream a -> Stream a -> Stream a
convolve s t =
  S.head s * S.head t <:> S.repeat (S.head s) * S.tail t + convolve (S.tail s) t
