import Data.Char (toUpper)
import Control.Monad (when)

-- HC11T1: Ask for name and greet
hc11t1 :: IO ()
hc11t1 = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"

-- HC11T2: Read line and print length
hc11t2 :: IO ()
hc11t2 = do
  line <- getLine
  putStrLn $ "Number of characters: " ++ show (length line)

-- HC11T3: Read number and print double
hc11t3 :: IO ()
hc11t3 = do
  putStrLn "Enter a number:"
  numStr <- getLine
  let num = read numStr :: Int
  putStrLn $ "Double: " ++ show (num * 2)

-- HC11T4: Read two lines and concatenate
hc11t4 :: IO ()
hc11t4 = do
  putStrLn "Enter first line:"
  line1 <- getLine
  putStrLn "Enter second line:"
  line2 <- getLine
  putStrLn $ "Concatenated: " ++ line1 ++ line2

-- HC11T5: Repeated input until "quit"
hc11t5 :: IO ()
hc11t5 = do
  putStrLn "Type something (or quit to exit):"
  input <- getLine
  when (input /= "quit") $ do
    putStrLn $ "You typed: " ++ input
    hc11t5

-- HC11T6: Read line, uppercase it, print
hc11t6 :: IO ()
hc11t6 = do
  line <- getLine
  putStrLn $ map toUpper line

-- HC11T7: Options menu
hc11t7 :: IO ()
hc11t7 = do
  putStrLn "Choose option:\n1) Greet\n2) Say Bye\n3) Quit"
  choice <- getLine
  case choice of
    "1" -> putStrLn "Hello there!"
    "2" -> putStrLn "Goodbye!"
    "3" -> putStrLn "Exiting..."
    _   -> putStrLn "Invalid option"
  when (choice /= "3") hc11t7

-- HC11T8: Read number and tell even or odd
hc11t8 :: IO ()
hc11t8 = do
  putStrLn "Enter a number:"
  numStr <- getLine
  let num = read numStr :: Int
  putStrLn $ if even num then "Even" else "Odd"

-- HC11T9: Read two numbers and print sum
hc11t9 :: IO ()
hc11t9 = do
  putStrLn "Enter first number:"
  n1 <- fmap read getLine :: IO Int
  putStrLn "Enter second number:"
  n2 <- fmap read getLine :: IO Int
  putStrLn $ "Sum: " ++ show (n1 + n2)

-- HC11T10: Read input and reverse string
hc11t10 :: IO ()
hc11t10 = do
  line <- getLine
  putStrLn $ reverse line


-- For quick testing, you can run, e.g., main = hc11t1
main :: IO ()
main = hc11t1
