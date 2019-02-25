import Data.Array.Accelerate              as A
import Data.Array.Accelerate.Interpreter              as I
--import Data.Array.Accelerate.LLVM.Native  as CPU
-- import Data.Array.Accelerate.LLVM.PTX     as GPU

dotp :: Acc (Vector Int) -> Acc (Vector Int) -> Acc (Scalar Int)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

dotp' :: Vector Int -> Vector Int -> Acc (Scalar Int)
dotp' xs ys = dotp (use xs) (use ys)
