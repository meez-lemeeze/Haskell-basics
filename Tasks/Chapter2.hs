import Data.List (sortBy)

-- Constants and Variables
myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

-- Function Definitions
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings a b = a ++ b

circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- Boolean expressions
bool1 = True && True
bool2 = False || False
bool3 = not False
bool4 = 10 < 5

-- Infix and Prefix examples
prefix1 = (+) 5 3
prefix2 = (*) 10 4
prefix3 = (&&) True False

infix1 = 7 + 2
infix2 = 6 * 5
infix3 = True && False

-- GHCi Type Expectations (for HC2T1)
-- 42            :: Num a => a
-- 3.14          :: Fractional a => a (defaults to Double)
-- "Haskell"     :: String
-- 'Z'           :: Char
-- True && False :: Bool

main :: IO ()
main = do
  putStrLn "\n--- GHCi Type Expectations ---"
  print (42 :: Integer)
  print (3.14 :: Double)
  print "Haskell"
  print 'Z'
  print (True && False)

  putStrLn "\n--- Function Tests ---"
  print (add 3 4)
  print (isEven 6)
  print (concatStrings "Hello, " "World!")
  print (circleArea 3)
  print (maxOfThree 10 25 15)

  putStrLn "\n--- Constants ---"
  print myAge
  print piValue
  print greeting
  print isHaskellFun

  putStrLn "\n--- Immutable Variables ---"
  -- Try uncommenting the next line to see Haskell reject reassignment
  -- myAge = 30

  putStrLn "\n--- Infix and Prefix Notation ---"
  print prefix1
  print prefix2
  print prefix3
  print infix1
  print infix2
  print infix3

  putStrLn "\n--- Int vs Integer ---"
  print smallNumber
  print bigNumber
  -- You can test this in GHCi: 2^64 :: Int

  putStrLn "\n--- Boolean Expressions ---"
  print bool1
  print bool2
  print bool3
  print bool4
