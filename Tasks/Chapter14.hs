{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import System.Random (randomRIO)
import Data.List (group, sort)
import Control.Exception (try, IOException)
import System.IO (readFile)

-- HC14T1 + HC14T2 + HC14T3 + HC14T4: basic IO, random, extensions, type applications

helloCabal :: IO ()
helloCabal = putStrLn "Hello, Cabal!"

printRandomNumber :: IO ()
printRandomNumber = do
  n <- randomRIO (1, 100)
  putStrLn $ "Random number between 1 and 100: " ++ show n

largeNumbers :: IO ()
largeNumbers = do
  let bigNum = 1_000_000_000
      hugeNum = 12_345_678_901_234
  putStrLn $ "Big number: " ++ show bigNum
  putStrLn $ "Huge number: " ++ show hugeNum

strToInt :: String -> Int
strToInt s = read @Int s

-- HC14T5: custom Result type + pattern matching with @

data Result a = Success a | Failure String deriving Show

printResult :: Result Int -> String
printResult s@(Success val) = "Success with value: " ++ show val ++ " (full: " ++ show s ++ ")"
printResult (Failure msg) = "Failure: " ++ msg

-- HC14T8: counts function to count character frequency in a string

counts :: String -> [(Char, Int)]
counts s = map (\grp -> (head grp, length grp)) . group . sort $ s

-- HC14T9: partial type signature example (already enabled)

countsPartial :: _ => String -> [(Char, Int)]
countsPartial = counts  -- just reuse counts, illustrating partial type signature

-- HC14T10: testing is done separately, not inside main

-- HC14T6+7: simulate library + main by having multiple functions here

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- fibonacci with recursion
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- HC14T7 (additional function): circle area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

-- HC14T8 (additional function): merge two sorted lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- HC14T9: read file with graceful error handling
readFileSafe :: FilePath -> IO ()
readFileSafe path = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left _  -> putStrLn $ "Error: Could not read file " ++ path
    Right contents -> putStrLn contents

-- Main program demonstrating all features
main :: IO ()
main = do
  helloCabal
  printRandomNumber
  largeNumbers

  putStrLn "\nEnter a number to convert (TypeApplications):"
  input <- getLine
  let n = strToInt input
  putStrLn $ "You entered (converted to Int): " ++ show n

  putStrLn "\nTesting Result type pattern matching:"
  putStrLn $ printResult (Success 42)
  putStrLn $ printResult (Failure "Something went wrong")

  putStrLn "\nFactorial of 5:"
  print $ factorial 5

  putStrLn "\nFirst 10 Fibonacci numbers:"
  print $ map fibonacci [0..9]

  putStrLn "\nCalculate circle area with radius 3.5:"
  print $ calculateCircleArea 3.5

  putStrLn "\nMerge sorted lists [1,3,5] and [2,4,6]:"
  print $ mergeLists [1,3,5] [2,4,6]

  putStrLn "\nCharacter counts for 'abracadabra':"
  print $ counts "abracadabra"

  putStrLn "\nReading a file 'test.txt' (try creating it, or expect error):"
  readFileSafe "test.txt"

  putStrLn "\nAll done!"
