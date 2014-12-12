module HughesTest where

import Hughes
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Monoid

-- This operation is especially slow for regular lists... but
-- actually quite fast for Hughes lists if you implement them 
-- right.
slowReverse :: [a] -> Hughes a
slowReverse = foldr (flip snoc) mempty

main :: IO ()
main = hspec $ do
  describe "runHughes and mkHughes are mutual inverses" $ do
    prop "runHughes . mkHughes = id" $ \l ->
      runHughes (mkHughes l) == (l :: [Int])
  describe "cons works" $ do
    prop "should work the same as consDumb" $ \l a -> 
      let 
          norm :: [Int] -> [Int]
          norm = runHughes . consDumb a . mkHughes
          new  :: [Int] -> [Int]
          new = runHughes . cons a . mkHughes
      in new l == norm l
  describe "append works" $ do
    prop "should work the same as consDumb" $ \l l' -> 
        let 
          norm :: [Int] -> [Int]
          norm = runHughes . appendDumb (mkHughes l') . mkHughes
          new  :: [Int] -> [Int]
          new = runHughes . mappend (mkHughes l') . mkHughes
        in new l == norm l
  describe "snoc works" $ do
    prop "should work the same as snocDumb" $ \l a -> 
        let 
          norm :: [Int] -> [Int]
          norm = runHughes . flip snocDumb a . mkHughes
          new  :: [Int] -> [Int]
          new = runHughes . flip snoc a . mkHughes
        in new l == norm l
  describe "efficiency" $ do
    it "should reverse efficiently" $
      let
        a = [1..100000]
        b = slowReverse a
      in runHughes b `shouldBe` reverse a

