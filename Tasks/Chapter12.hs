-- HC12T1: Print welcome message
hc12t1 :: IO ()
hc12t1 = putStrLn "Welcome to Haskell Programming!"

-- HC12T2: Add two numbers and print result
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

hc12t2 :: IO ()
hc12t2 = do
  let result = addTwoNumbers 5 7
  putStrLn $ "Sum: " ++ show result

-- HC12T3: Factorial function (recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

hc12t3 :: IO ()
hc12t3 = do
  putStrLn "Factorial of 5 is:"
  print $ factorial 5

-- HC12T4: First 10 Fibonacci numbers (recursive)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

hc12t4 :: IO ()
hc12t4 = print $ map fib [0..9]

-- HC12T5: Palindrome check function
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

hc12t5 :: IO ()
hc12t5 = do
  putStrLn "Enter a string:"
  s <- getLine
  putStrLn $ if isPalindrome s then "It's a palindrome." else "Not a palindrome."

-- HC12T6: Read list of integers and print sorted
hc12t6 :: IO ()
hc12t6 = do
  putStrLn "Enter numbers separated by spaces:"
  line <- getLine
  let nums = map read (words line) :: [Int]
  putStrLn $ "Sorted: " ++ show (quickSort nums)

-- Helper quickSort function for HC12T6
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

-- HC12T7: Calculate circle area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

hc12t7 :: IO ()
hc12t7 = do
  let r = 3.0
  putStrLn $ "Area of circle with radius " ++ show r ++ " is " ++ show (calculateCircleArea r)

-- HC12T8: Merge two sorted lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

hc12t8 :: IO ()
hc12t8 = do
  let a = [1,3,5,7]
  let b = [2,4,6,8]
  putStrLn $ "Merged list: " ++ show (mergeLists a b)

-- HC12T9: Read file content safely
import System.IO.Error (catchIOError)

hc12t9 :: IO ()
hc12t9 = do
  putStrLn "Enter filename:"
  fname <- getLine
  catchIOError
    (readFile fname >>= putStrLn)
    (\e -> putStrLn $ "Error reading file: " ++ show e)

-- HC12T10: Define a module for math operations (in separate file MathOps.hs)
-- Content of MathOps.hs:
-- module MathOps (square, cube) where
-- square :: Num a => a -> a
-- square x = x * x
-- cube :: Num a => a -> a
-- cube x = x * x * x

-- Usage in main file:
-- import MathOps
-- hc12t10 :: IO ()
-- hc12t10 = do
--   putStrLn $ "Square of 3: " ++ show (square 3)
--   putStrLn $ "Cube of 3: " ++ show (cube 3)

-- For demonstration here, we define the functions inline:

square :: Num a => a -> a
square x = x * x

cube :: Num a => a -> a
cube x = x * x * x

hc12t10 :: IO ()
hc12t10 = do
  putStrLn $ "Square of 3: " ++ show (square 3)
  putStrLn $ "Cube of 3: " ++ show (cube 3)

-- To test, you can call any of these in main, e.g.:
main :: IO ()
main = hc12t1
