{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ConCat where


import Data.Complex
import Data.List.NonEmpty as NE


infixl 7 :*
type (:*)  = (,)

type Unop   a = a -> a

omega :: RealFloat a => Int -> Complex a
omega n = cis (- 2 * pi / fromIntegral n)
-- omega n = exp (0 :+ (- 2 * pi / fromIntegral n))
-- omega n = exp (- 2 * (0:+1) * pi / fromIntegral n)
{-# INLINE omega #-}

omegas :: (RealFloat a) => Int -> NonEmpty (NonEmpty (Complex a))
omegas = fmap powers . powers . omega


powers :: (Num a) => a -> NonEmpty a
powers = NE.scanl (*) 1 . NE.repeat
{-# INLINE powers #-}

