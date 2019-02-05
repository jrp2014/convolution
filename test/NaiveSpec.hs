module NaiveSpec
  ( spec
  ) where

import Naive

import Test.Hspec
import Test.QuickCheck

import qualified Data.Array as A
import qualified Data.Vector as V

hs :: [Int]
hs = [1,2,3,4]

xs :: [Int]
xs = [1,2,3,4,5]

hxs :: [Int]
hxs = [ 1, 4, 10, 20, 30, 34, 31, 20 ]

hsA :: A.Array Int Int
hsA = A.listArray (0, length hs - 1) hs
xsA :: A.Array Int Int
xsA = A.listArray (0, length xs - 1) xs
hxsA :: A.Array Int Int
hxsA = A.listArray (0, length hxs - 1) hxs

spec :: Spec
spec = do
  describe "Basic" $ do
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolve" ) $ do
      convolve hs xs `shouldBe` hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveV" ) $ do
      convolveV (V.fromList hs) (V.fromList xs) `shouldBe` (V.fromList hxs)
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveA") $ do
      convolveA hsA xsA `shouldBe` hxsA
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveR" ) $ do
      convolveR hs xs `shouldBe` hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveL" ) $ do
      convolveL hs xs `shouldBe` hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolve'" ) $ do
      convolve' hs xs `shouldBe` hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using parConvolve" ) $ do
      parConvolve hs xs `shouldBe` hxs

