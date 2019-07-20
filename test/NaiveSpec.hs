{-# LANGUAGE FlexibleContexts #-}

module NaiveSpec
  ( spec
  ) where

import Naive

import Semi

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Vector ()

import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- Don't use the quickcheck-instances version, as it picks arbitrary bounds
-- rather than indexing from zero
instance (A.Ix a, Integral a, Arbitrary b) => Arbitrary (A.Array a b) where
  arbitrary =
    (\x -> A.listArray (0, fromIntegral (length x - 1)) x) <$> arbitrary

instance (CoArbitrary b) => CoArbitrary (A.Array a b) where
  coarbitrary = coarbitrary . A.elems

instance (UA.Ix i, Integral i, UA.IArray UA.UArray a, Arbitrary a) =>
         Arbitrary (UA.UArray i a) where
  arbitrary =
    (\x -> UA.listArray (0, fromIntegral (length x - 1)) x) <$> arbitrary

instance (UA.Ix i, UA.IArray UA.UArray a, CoArbitrary a) =>
         CoArbitrary (UA.UArray i a) where
  coarbitrary = coarbitrary . UA.elems

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

hsUA :: UA.UArray Int Int
hsUA = UA.listArray (0, length hs - 1) hs

xsUA :: UA.UArray Int Int
xsUA = UA.listArray (0, length xs - 1) xs

hxsUA :: UA.UArray Int Int
hxsUA = UA.listArray (0, length hxs - 1) hxs

spec :: Spec
spec = do
  describe "Basic" $ do
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolve") $
      convolve hs xs `shouldBe`
      hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveV") $
      convolveV (V.fromList hs) (V.fromList xs) `shouldBe`
      V.fromList hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveUV") $
      convolveUV (UV.fromList hs) (UV.fromList xs) `shouldBe`
      UV.fromList hxs
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveA") $
      convolveA hsA xsA `shouldBe`
      hxsA
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using convolveUA") $
      convolveUA hsUA xsUA `shouldBe`
      hxsUA
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
    it ("convolves " ++ show hs ++ " with " ++ show xs ++ " using (#)") $ hs #
      xs `shouldBe`
      hxs
    it
      ("convolves " ++ show hs ++ " with " ++ show xs ++
       " using Conal Elliot's <.>") $
      hs <.>
      xs `shouldBe`
      hxs
  describe "Commutativity" $ do
    it "of convolve" $ property $ \(NonEmpty pxs) (NonEmpty phs) ->
      convolve (pxs :: [Int]) (phs :: [Int]) == convolve phs pxs
    it "of convolveV" $ property $ \pxs phs ->
      not (V.null pxs) && not (V.null phs) ==>
      convolveV (pxs :: V.Vector Int) (phs :: V.Vector Int) ==
      convolveV phs pxs
    it "of convolveUV" $ property $ \pxs phs ->
      not (UV.null pxs) && not (UV.null phs) ==>
      convolveUV (pxs :: UV.Vector Int) (phs :: UV.Vector Int) ==
      convolveUV phs pxs
    it "of convolveA" $ property $ \pxs phs ->
      let (_, u) = A.bounds pxs
          (_, u') = A.bounds phs
       in 0 < u && 0 < u' ==>
          convolveA (pxs :: A.Array Int Int) (phs :: A.Array Int Int) ==
          convolveA phs pxs
    it "of convolveUA" $ property $ \pxs phs ->
      let (_, u) = UA.bounds pxs
          (_, u') = UA.bounds phs
       in 0 < u && 0 < u' ==>
          convolveUA (pxs :: UA.UArray Int Int) (phs :: UA.UArray Int Int) ==
          convolveUA phs pxs
    it "of convolveR" $ property $ \(NonEmpty pxs) (NonEmpty phs) ->
      not (null pxs) && not (null phs) ==>
      convolveR (pxs :: [Int]) (phs :: [Int]) ==
      convolveR phs pxs
    it "of convolveL" $ property $ \(NonEmpty pxs) (NonEmpty phs) ->
      convolveL (pxs :: [Int]) (phs :: [Int]) == convolveL phs pxs
    it "of convolve'" $ property $ \(NonEmpty pxs) (NonEmpty phs) ->
      convolve' (pxs :: [Int]) (phs :: [Int]) == convolve' phs pxs
    it "of parConvolve" $ property $ \(NonEmpty pxs) (NonEmpty phs) ->
      parConvolve (pxs :: [Int]) (phs :: [Int]) == parConvolve phs pxs
    it "of convolveS" $ property $ \(NonEmpty pxs) (NonEmpty phs) ->
      convolveS (pxs :: [Int]) (phs :: [Int]) == convolveS phs pxs
    it "of (#)" $ property $ \(NonEmpty pxs) (NonEmpty phs) ->
      (pxs :: [Int]) # (phs :: [Int]) == phs # pxs
    it "of Conal Elliot's <.>" $ property $ \(NonEmpty pxs) (NonEmpty phs) ->
      convolve (pxs :: [Int]) (phs :: [Int]) == convolve phs pxs
