-------------------------------------------------------------
--
-- Author: Joao H de A Franco (jhafranco@acm.org)
--
-- Description: DFT implementation in Haskell
--
-- Date: 2013-Mar-28
--
-- License: Attribution-NonCommercial-ShareAlike 3.0 Unported
--          (CC BY-NC-SA 3.0)
--
-------------------------------------------------------------
module Aux.DFT (dft,idft) where
import Data.Complex (Complex((:+)))

dft, idft :: [Complex Float] -> [Complex Float]
dft = dft' 1
idft = dft' (-1)
           
dft' :: Float -> [Complex Float] -> [Complex Float]
dft' e x = let t = length x
               y = 1 / fromIntegral t
               dft'' k = let w = fromIntegral k * 2 * e * pi * y
                         in (sqrt y :+ 0) *
                             sum (zipWith (\n z -> z * exp (0 :+ n * w)) [0..] x)                         
           in map dft'' [0..t-1]


