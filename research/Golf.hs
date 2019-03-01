

(#) :: Num b => [b] -> [b] -> [b]
(a : b) # c = zipWith (+) (0 : b # c) $ map (a *) c ++ [] # b
_       # c = 0 <$ c
