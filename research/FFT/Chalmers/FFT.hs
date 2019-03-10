import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal
import Criterion.Main
import Control.Parallel
import Control.Monad.Par  
import Control.Parallel.Strategies

-- file given.hs for use with Lab 1 Part 1 of the Chalmers PFP Course
-- Please write your names in the file if submitting it


-- generating input for FFT or DFT. Borrowed from Simon Marlow I believe.
mX, mY, sdX, sdY :: Double
mX = 0
mY = 0
sdX = 0.5
sdY = 1.5    

generate2DSamplesList :: Int           -- number of samples to generate
                  -> Double -> Double    -- X and Y mean
                  -> Double -> Double    -- X and Y standard deviations
                  -> IO [Complex Double]
generate2DSamplesList n mx my sdx sdy = do
  gen <- getStdGen
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return $ zipWith (:+) (take n xsamples) ysamples




-- Task 1
divConq :: (prob -> Bool)              -- is the problem indivisible?
            -> (prob -> [prob])        -- split
            -> ([sol] -> sol)          -- join
            -> (prob -> sol)           -- solve a sub-problem
            -> (prob -> sol)

divConq indiv split join f prob = undefined



-- Task 2


-- twiddle factors
tw :: Int -> Int -> Complex Double
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)

dft :: [Complex Double] -> [Complex Double]
dft xs = [ sum [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
  where
    n = length xs
    n' = n-1



-- In case you are wondering, this is the Decimation in Frequency (DIF) 
-- radix 2 Cooley-Tukey FFT

fft :: [Complex Double] -> [Complex Double]
fft [a] = [a]
fft as = interleave ls rs
  where
    (cs,ds) = bflyS as
    ls = fft cs
    rs = fft ds

interleave :: [a] -> [a] -> [a]
interleave [] bs = bs
interleave (a:as) bs = a : interleave bs as

bflyS :: [Complex Double] -> ([Complex Double], [Complex Double])
bflyS as = (los,rts)
  where
    (ls,rs) = halve as
    los = zipWith (+) ls rs
    ros = zipWith (-) ls rs
    rts = zipWith (*) ros [tw (length as) i | i <- [0..length ros - 1]]


-- missing from original file
halve :: [a] -> ([a], [a])
halve as = splitAt n' as
  where
    n' = div (length as + 1) 2






