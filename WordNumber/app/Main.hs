module Main where

import Test.Hspec
import Test.QuickCheck

import Data.List (sort)
import Data.Char (toUpper)

import WordNumber (digitToWord, digits, wordNumber)

floatGen :: Gen Float
floatGen = arbitrary

half :: Float -> Float
half x = x / 2

prop_half :: Property
prop_half =
  forAll floatGen (\i -> half (i * 2) == i)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

intListGen :: Gen [Integer]
intListGen = arbitrary

prop_sorted :: Property
prop_sorted =
  forAll intListGen (\iList -> listOrdered (sort iList))

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

prop_associative :: Property
prop_associative =
  forAll (genThreeple :: Gen (Int, Int, Int)) (\(x, y, z) -> x + (y + z) == (x + y) + z)

prop_associative_mult :: Property
prop_associative_mult =
  forAll (genThreeple :: Gen (Int, Int, Int)) (\(x, y, z) -> x * (y * z) == (x * y ) * z)

gen2Int :: Gen (Int, Int)
gen2Int = do
  a <- elements [1..100]
  b <- elements [1..100]
  return (a, b)

prop_remQuot :: Property
prop_remQuot =
  forAll gen2Int (\(x, y) -> (quot x y) * y + (rem x y) == x)

prop_divMod :: Property
prop_divMod =
  forAll gen2Int (\(x, y) -> (div x y) * y + (mod x y) == x)

prop_assocExponential :: Property
prop_assocExponential =
  forAll (genThreeple :: Gen (Int, Int, Int)) (\(x, y, z) -> x ^ (y ^ z) == (x ^ y) ^ z)

prop_commutativeExponential :: Property
prop_commutativeExponential =
  forAll gen2Int (\(x, y) -> x ^ y == y ^ x)

prop_reverseList :: Property
prop_reverseList =
  forAll intListGen (\iList -> (reverse (reverse iList)) == iList)

--8 CoArbitrary?

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

prop_foldRColonPlusPlus :: Property
prop_foldRColonPlusPlus =
  forAll (genTuple :: Gen ([Int], [Int])) (\(x, y) -> foldr (:) y x == x ++ y)

genListIntLists :: Gen [[Int]]
genListIntLists = arbitrary

prop_foldRConcat :: Property
prop_foldRConcat =
  forAll genListIntLists (\lls -> foldr (++) [] lls == concat lls)

prop_takeLength :: Property
prop_takeLength =
  forAll (genTuple :: Gen (Int, [Int])) (\(n, xs) -> length (take n xs) == n)

prop_readShow :: Property
prop_readShow =
  forAll floatGen (\f -> read (show f) == f)

prop_squareRoot :: Property
prop_squareRoot =
  forAll floatGen (\f -> sqrt (f * f) == f)
--Negative numbers + imprecision of floating point

--Idempotence
twice f = f . f
fourTimes = twice . twice

genString :: Gen String
genString = arbitrary

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (h : t) = (toUpper h : t)

prop_idempotence1 :: Property
prop_idempotence1 =
  forAll genString (\s -> capitalizeWord s == twice capitalizeWord s &&
                          capitalizeWord s == fourTimes capitalizeWord s)

prop_idempotence2 :: Property
prop_idempotence2 =
  forAll genString (\s -> sort s == twice sort s &&
                          sort s == fourTimes sort s)

--Make a Gen random generator for the datatype
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = elements [Fulse, Frue]

runQc :: IO ()
runQc = do
  quickCheck prop_half
  quickCheck prop_sorted
  quickCheck prop_associative
  quickCheck prop_associative_mult
  quickCheck prop_remQuot
  quickCheck prop_divMod
  quickCheck prop_assocExponential
  quickCheck prop_commutativeExponential
  quickCheck prop_reverseList
  quickCheck prop_foldRColonPlusPlus
  quickCheck prop_foldRConcat
  quickCheck prop_takeLength
  quickCheck prop_readShow
  quickCheck prop_squareRoot
  quickCheck prop_idempotence1
  quickCheck prop_idempotence2

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
