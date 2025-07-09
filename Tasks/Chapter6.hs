-- HC6T1: Factorial using recursion
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC6T2: Fibonacci using recursion
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- HC6T3: Sum using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- HC6T4: Product using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

-- HC6T5: Reverse list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- HC6T6: Check if element exists in list
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists x (y:ys)
  | x == y    = True
  | otherwise = elementExists x ys

-- HC6T7: Length of list
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- HC6T8: Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)
  | even x    = x : filterEvens xs
  | otherwise = filterEvens xs

-- HC6T9: Map implementation
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

-- HC6T10: Get digits of a number recursively
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- Main to test all the above
main :: IO ()
main = do
  putStrLn "\n--- HC6T1: Factorial ---"
  print (factorial 5)   -- 120

  putStrLn "\n--- HC6T2: Fibonacci ---"
  print (fibonacci 10)  -- 55

  putStrLn "\n--- HC6T3: Sum with foldr ---"
  print (sumList [1..5])  -- 15

  putStrLn "\n--- HC6T4: Product with foldl ---"
  print (productList [1..5])  -- 120

  putStrLn "\n--- HC6T5: Reverse List ---"
  print (reverseList [1,2,3,4])  -- [4,3,2,1]

  putStrLn "\n--- HC6T6: Element Exists ---"
  print (elementExists 3 [1,2,3,4])  -- True
  print (elementExists 5 [1,2,3,4])  -- False

  putStrLn "\n--- HC6T7: List Length ---"
  print (listLength [10,20,30,40])  -- 4

  putStrLn "\n--- HC6T8: Filter Evens ---"
  print (filterEvens [1..10])  -- [2,4,6,8,10]

  putStrLn "\n--- HC6T9: Map (Double Elements) ---"
  print (mapList (*2) [1,2,3])  -- [2,4,6]

  putStrLn "\n--- HC6T10: Digits of Number ---"
  print (digits 12345)  -- [1,2,3,4,5]
