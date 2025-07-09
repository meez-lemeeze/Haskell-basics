-- HC1 All-in-One Haskell Tasks

import Data.List (sortBy)

-- Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- Task 2: Pure Function Example
circleArea :: Float -> Float
circleArea r = pi * r * r

-- Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- Task 4: Composing a Function to Process Player Data
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers = map fst

sortByScore :: [Player] -> [Player]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

takeN :: Int -> [Int]
takeN n = take n infiniteNumbers

-- Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Task 7: Converting Fahrenheit to Celsius
fToC :: Float -> Float
fToC f = (f - 32) * 5 / 9

-- Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main Function to Test All Tasks
main :: IO ()
main = do
  putStrLn "=== Task 1: Function Composition ==="
  print (double 4)              -- 8
  print (increment 4)           -- 5
  print (doubleThenIncrement 4) -- 9

  putStrLn "\n=== Task 2: Pure Function ==="
  print (circleArea 3)          -- 28.274334

  putStrLn "\n=== Task 3: Greater Than 18 ==="
  print (greaterThan18 20)      -- True
  print (greaterThan18 10)      -- False

  putStrLn "\n=== Task 4: Top Three Players ==="
  let players = [("Alice", 40), ("Bob", 50), ("Charlie", 30), ("Dave", 60)]
  print (getTopThreePlayers players) -- ["Dave", "Bob", "Alice"]

  putStrLn "\n=== Task 5: Laziness ==="
  print (takeN 10)              -- [1..10]

  putStrLn "\n=== Task 6: Add Numbers ==="
  print (addNumbers 5 7)        -- 12

  putStrLn "\n=== Task 7: Fahrenheit to Celsius ==="
  print (fToC 98.6)             -- 37.0

  putStrLn "\n=== Task 8: Apply Twice ==="
  print (applyTwice (+3) 4)     -- 10
  print (applyTwice (*2) 3)     -- 12
