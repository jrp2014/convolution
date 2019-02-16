{- Basic operations on power series and polynomials
   represented by lists, practical version, as described
   in http://www.cs.dartmouth.edu/~doug/powser.html

   Doug McIlroy, July 2007
-}
default (Integer, Rational, Double)

infixr 9 #

instance (Num a, Eq a) => Num [a] where
  fromInteger c = [fromInteger c]
  negate = map negate
  (f:ft) + (g:gt) = f + g : ft + gt
  fs + [] = fs
  [] + gs = gs
  (0:ft) * gs = 0 : ft * gs -- caveat: 0*diverge = 0
  fs * (0:gt) = 0 : fs * gt
  (f:ft) * gs@(g:gt) = f * g : ft * gs + [f] * gt
  _ * _ = []

instance (Fractional a, Eq a) => Fractional [a] where
  fromRational c = [fromRational c]
  (0:ft) / gs@(0:gt) = ft / gt
  (0:ft) / gs@(g:gt) = 0 : ft / gs
  (f:ft) / gs@(g:gt) = f / g : (ft - [f / g] * gt) / gs
  [] / (0:gt) = [] / gt
  [] / (g:gt) = []
  _ / _ = error "improper power series division"

(#) :: (Eq a, Num a) => [a] -> [a] -> [a]
(f:ft) # gs@(0:gt) = f : gt * ft# gs
(f:ft) # gs@(g:gt) = [f] + gs * ft# gs -- ft must be polynomial
[] # _ = []
(f:_) # [] = [f]

revert :: (Fractional a, Eq a) => [a] -> [a]
revert (_:0:_) = error "revert f where f'(0)==0"
revert (0:ft) = rs
  where
    rs = 0 : 1 / ft# rs
revert [f, f'] = [-f / f', 1 / f']
revert _ = error "revert f where f(0)/=0"

int :: Fractional a => [a] -> [a]
int fs = 0 : zipWith (/) fs (map fromInteger [1 ..])

diff :: Num c => [c] -> [c]
diff (_:ft) = zipWith (*) ft (map fromInteger [1 ..])

tans :: [Rational]
tans = revert (int (1 / (1 : 0 : 1)))

sins :: [Rational]
sins = int coss

coss :: [Rational]
coss = 1 - int sins

pascal :: [[Rational]]
pascal = 1 / [1, -[1, 1]]
