{-# LANGUAGE GeneralizedNewtypeDeriving,
             TupleSections
  #-}

import Control.Monad
import Data.Complex
import System.Random
import Text.Printf
import Test.QuickCheck.Arbitrary

{-|
A custom double precision floating point type, which overides the 'Show'
and 'Eq' instances, in order to:

    * Limit precision to 3 places after the decimal.

    * Print ridiculously small numbers as simply zero.

    * Define equality as difference being less than some threshold.
-}
newtype PrettyDouble = PrettyDouble {
    uglyDouble :: Double
  } deriving (Ord, Fractional, Floating, Real, RealFrac, RealFloat, Enum, Arbitrary, CoArbitrary, Random)

instance Show PrettyDouble where
    show = printf "%6.3g" . zeroThresh . uglyDouble
        where zeroThresh y =
                if abs y < 1.0e-10
                then 0.0
                else y

instance Eq PrettyDouble where
    z1' == z2' = abs (z1 - z2) < 1.0e-3
        where z1 = uglyDouble z1'
              z2 = uglyDouble z2'
              
instance Num PrettyDouble where
    x + y         = PrettyDouble $ uglyDouble x + uglyDouble y
    x - y         = PrettyDouble $ uglyDouble x - uglyDouble y
    x * y         = PrettyDouble $ uglyDouble x * uglyDouble y
    abs x         = PrettyDouble $ abs $ uglyDouble x
    signum x      = PrettyDouble $ signum $ uglyDouble x
    fromInteger n = PrettyDouble $ fromIntegral n

-- The DFT forms the specification for the FFT.
dft :: RealFloat a => [Complex a] -> [Complex a]
dft xs = [ sum [ x * exp((0.0 :+ (-1.0)) * 2 * pi / lenXs * fromIntegral(k * n))
                 | (x, n) <- zip xs [0..]
               ]
           | k <- [0..(length xs - 1)]
         ]
    where lenXs = fromIntegral $ length xs

-- FFT implementation
-- Note my reversal of Ross' even/odd convention, below.
-- I did this, to remain consistent with the explanation, above, which came from a pre-existing work.
fft :: RealFloat b => Hom (Complex b) (Complex b)
fft = id :&: proc (e, o) -> do
                e' <- fft           -< e
                o' <- doTwiddle fft -< o
                unriffle -< f (e', o')
    where f (x, y) = (x + y, x - y)

-- From Ross:
twiddle = roots (-1)

roots :: RealFloat b => Complex b -> Hom () (Complex b)
roots r = const 1 :&: proc _ -> do
            x <- roots r' -< ()
            unriffle -< (x, x*r')
 where
   r' = if imagPart s >= 0 then -s else s
   s  = sqrt r
-- end From Ross

doTwiddle :: RealFloat b => Hom (Complex b) (Complex b) -> Hom (Complex b) (Complex b)
doTwiddle f = arr (,()) >>> (f *** twiddle) >>> arr (uncurry (*))

inSucc :: Hom (Pair b) (Pair b) -> Hom b b
inSucc f = id :&: f

-- Testing machinery
-- Conversion to list, for comparison to specification (i.e. - DFT)
btToList :: BalTree b -> [b]
btToList = btToList' . convertTree

-- Tree of pairs seems to be much more convenient, in some circumstances.
data BalTree' b = Zero' b
                | Succ' (Pair (BalTree' b))
    deriving (Show)
    
apply' :: Hom a b -> BalTree' a -> BalTree' b
apply' (f :&: fs) (Zero' x)        = Zero' (f x)
apply' (f :&: fs) (Succ' (t1, t2)) = Succ' ((apply' (f :&: fs) t1), (apply' (f :&: fs) t2))

btToList' :: BalTree' b -> [b]
btToList' (Zero' x)        = [x]
btToList' (Succ' (t1, t2)) = btToList' t1 ++ (btToList' t2)

-- We'll need conversion functions back and forth.
-- (I probably could have just lifted this from Conal Elliott's recent work,
-- but decided it'd be a good exercise to do it myself.)
invertTree :: BalTree' b -> BalTree b
invertTree (Zero' x)        = Zero x
invertTree (Succ' (t1, t2)) = joinTrees (invertTree t1, invertTree t2)

joinTrees :: Pair (BalTree b) -> BalTree b
joinTrees (Zero x,   Zero y) = Succ (Zero (x, y))
joinTrees (Succ t1, Succ t2) = Succ (joinTrees (t1, t2))

convertTree :: BalTree b -> BalTree' b
convertTree (Zero x) = Zero' x
convertTree (Succ t) = Succ' $ (convertTree `prod` convertTree) (transposeTree t)

transposeTree :: BalTree (Pair b) -> Pair (BalTree b)
transposeTree (Zero (x, y)) = (Zero x, Zero y)
transposeTree (Succ t)      = (Succ `prod` Succ) (transposeTree t)

unriffleTree :: BalTree b -> BalTree b
unriffleTree (Zero x) = Zero x
unriffleTree (Succ t) = Succ (apply unriffle t)

-- Build a tree of depth 'n', taking values from supplied list, sequentially.
-- This is awfully clunky, moving through an incorrectly assembled Pair of BalTrees first.
-- However, I couldn't figure out a better way of coping with an infinitely long input list.
buildTree :: Int -> [b] -> BalTree b
buildTree n = invertTree . buildTree' n

buildTree' :: Int -> [b] -> BalTree' b
buildTree' _ []     = error "Exhausted input!"
buildTree' 0 (x:xs) = Zero' x
buildTree' n xs     = mergeTrees ((buildTree' (n-1) (evens xs)), (buildTree' (n-1) (odds xs)))

mergeTrees :: Pair (BalTree' b) -> BalTree' b
mergeTrees (Zero' x,  Zero' y)              = Succ' (Zero' x, Zero' y)
mergeTrees (Succ' (t1, t2), Succ' (t3, t4)) = Succ' (mergeTrees (t1, t3), mergeTrees (t2, t4))
mergeTrees (_,        _)                    = error "Attempt to merge unbalanced trees!"

evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : (odds xs)

odds :: [a] -> [a]
odds []     = []
odds (x:xs) = evens xs

twiddleRef :: RealFloat b => [Complex b] -> [Complex b]
twiddleRef xs = map (uncurry (*)) $ zip xs [theta ** m | m <- map ((:+ 0.0) . fromIntegral) [0..(m' - 1)]]
                                            where theta = cis (-pi / (fromIntegral m'))
                                                  m'    = length xs

-- Some test sequences:
-- Needed, to exercise twiddle().
ones  = repeat (PrettyDouble 1.0)
-- Odd triangle wave, at fundamental frequency.
test1 = [0.0, 0.25, 0.5, 0.75, 1.0, 0.75, 0.5, 0.25, 0.0, -0.25, -0.5, -0.75, -1.0, -0.75, -0.5, -0.25] :: [PrettyDouble]

main = do
  g <- getStdGen
  let rands = (randoms g :: [PrettyDouble])
  forM_ [0..5] (\n -> do
      let t = buildTree n (map (:+ 0.0) rands)
      putStrLn $ "Tree depth: " ++ (show n)
      putStr "\tTesting twiddles..."
      print $ btToList (apply (arr (,()) >>> (idA *** twiddle) >>> arr (uncurry (*))) t) == twiddleRef (btToList t)
      putStr "\tTesting FFT..."
      print $ btToList (apply fft t) == (dft (btToList t))
      )
