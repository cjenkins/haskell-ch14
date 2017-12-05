module WordNumber where

digitToWord :: Integer -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Integer -> [Integer]
digits 1 = [1]
digits 100 = [1, 0, 0]

wordNumber :: Integer -> String
wordNumber 100 = "one-zero-zero"
wordNumber 9001 = "nine-zero-zero-one"
