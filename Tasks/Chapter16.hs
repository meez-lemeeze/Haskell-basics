module HC16 where

import Data.Char (toUpper)
import Data.List (foldl')

-- HC16T1: Reverse a string
reverseString :: String -> String
reverseString = reverse

-- HC16T2: Check if a string is a palindrome
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- HC16T3: Calculate factorial (recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n
  | n > 0     = n * factorial (n - 1)
  | otherwise = error "factorial: negative input"

-- HC16T4: Filter only even numbers from a list
filterEven :: [Int] -> [Int]
filterEven = filter even

-- HC16T5: Convert all characters in a string to uppercase
toUpperCase :: String -> String
toUpperCase = map toUpper

-- HC16T6: Return the nth Fibonacci number (recursive)
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n
  | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
  | otherwise = error "fibonacci: negative input"

-- HC16T7: Check if element exists in a list
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

-- HC16T8: Insertion sort for list of integers
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

-- HC16T9: Remove duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl' addIfNotPresent []
  where
    addIfNotPresent seen x
      | x `elem` seen = seen
      | otherwise     = seen ++ [x]

-- HC16T10: Count frequency of each character in a string
charFrequency :: String -> [(Char, Int)]
charFrequency = foldl' countChar []
  where
    countChar [] c = [(c, 1)]
    countChar ((ch, n):xs) c
      | ch == c   = (ch, n + 1) : xs
      | otherwise = (ch, n) : countChar xs c
