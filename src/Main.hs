module Main where

import GHC.Generics (M1 (unM1))
import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

rcdSumMult :: (Eq a, Num a) => a -> a -> a
rcdSumMult x y
  | y == 0 = 0
  | otherwise = x + rcdSumMult x (y - 1)

-- This is our main function
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
  describe "Multiplication" $ do
    it "5 times 6 is 30" $ do
      rcdSumMult 5 6 `shouldBe` 30
    it "7 times 1 should be 7" $ do
      rcdSumMult 7 1 `shouldBe` 7
    it "8 times 0 should be 0" $ do
      rcdSumMult 8 0 `shouldBe` 0
    it "x times 0 should always be 0" $ do
      property $ \x -> rcdSumMult (x :: Int) 0 == 0
  describe "QuickCheck" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
