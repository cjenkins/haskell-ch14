module Addition where

import Test.Hspec
import Test.QuickCheck

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

multiplyBy :: (Num a, Eq a) => a -> a -> a
multiplyBy num mult = go num mult num
  where go num mult sum
          | mult == 0 = 0
          | mult == 1 = sum
          | otherwise = go num (mult - 1) (sum + num)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "QuickCheck" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "DivideBy" $ do
    it "15 divided by 3 is 5" $ do
      divideBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      divideBy 22 5 `shouldBe` (4, 2)
  describe "MultiplyBy" $ do
    it "15 times 0 is 0" $ do
      multiplyBy 15 0 `shouldBe` 0
    it "15 times 1 is 15" $ do
      multiplyBy 15 1 `shouldBe` 15
    it "15 times 2 is 30" $ do
      multiplyBy 15 2 `shouldBe` 30
