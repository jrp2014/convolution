module NaiveSpec
  ( spec
  ) where

import Naive

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Vector ()

import qualified Data.Array as A
import qualified Data.Vector as V

-- Don't use the quickcheck-instances version, as it picks arbitrary bounds
--  rather than indexing from zero
instance (A.Ix a, Integral a, Arbitrary b) => Arbitrary (A.Array a b) where
  arbitrary =
    (\x -> A.listArray (0, fromIntegral (length x - 1)) x) <$> arbitrary

instance (CoArbitrary b) => CoArbitrary (A.Array a b) where
  coarbitrary = coarbitrary . A.elems

hs :: [Int]
hs = [1, 2, 3, 4, 5]

xs :: [Int]
xs = [1, 2, 3, 4]

hxs :: [Int]
hxs = [1, 4, 10, 20, 30, 34, 31, 20]

hsA :: A.Array Int Int
hsA = A.listArray (0, length hs - 1) hs

xsA :: A.Array Int Int
xsA = A.listArray (0, length xs - 1) xs

hxsA :: A.Array Int Int
hxsA = A.listArray (0, length hxs - 1) hxs

spec :: Spec
spec = do
  describe "Basic" $ do
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolve") $
      convolve hs xs `shouldBe`
      hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveV") $
      convolveV (V.fromList hs) (V.fromList xs) `shouldBe`
      V.fromList hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveA") $
      convolveA hsA xsA `shouldBe`
      hxsA
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveR") $
      convolveR hs xs `shouldBe`
      hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveL") $
      convolveL hs xs `shouldBe`
      hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolve'") $
      convolve' hs xs `shouldBe`
      hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using parConvolve") $
      parConvolve hs xs `shouldBe`
      hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveS") $
      convolveS hs xs `shouldBe`
      hxs
  describe "Commutativity" $ do
    it "of convolve" $ property $ \pxs phs ->
      not (null pxs) && not (null phs) ==>
      convolve (pxs :: [Int]) (phs :: [Int]) ==
      convolve phs pxs
    it "of convolveV" $ property $ \pxs phs ->
      not (V.null pxs) && not (V.null phs) ==>
      convolveV (pxs :: V.Vector Int) (phs :: V.Vector Int) ==
      convolveV phs pxs
    it "of convolveA" $ property $ \pxs phs ->
      let (_, u) = A.bounds pxs
          (_, u') = A.bounds phs
       in 0 < u && 0 < u' ==>
          convolveA (pxs :: A.Array Int Int) (phs :: A.Array Int Int) ==
          convolveA phs pxs
    it "of convolveR" $ property $ \pxs phs ->
      not (null pxs) && not (null phs) ==>
      convolveR (pxs :: [Int]) (phs :: [Int]) ==
      convolveR phs pxs
    it "of convolveL" $ property $ \pxs phs ->
      not (null pxs) && not (null phs) ==>
      convolveL (pxs :: [Int]) (phs :: [Int]) ==
      convolveL phs pxs
    it "of convolve'" $ property $ \pxs phs ->
      not (null pxs) && not (null phs) ==>
      convolve' (pxs :: [Int]) (phs :: [Int]) ==
      convolve' phs pxs
    it "of parConvolve" $ property $ \pxs phs ->
      not (null pxs) && not (null phs) ==>
      parConvolve (pxs :: [Int]) (phs :: [Int]) ==
      parConvolve phs pxs
    it "of convolveS" $ property $ \pxs phs ->
      not (null pxs) && not (null phs) ==>
      convolveS (pxs :: [Int]) (phs :: [Int]) ==
      convolveS phs pxs
