import Numeric (showHex)
import Text.Printf (printf)

-- HC3T1 - Check if number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber x =
  if x > 0 then "Positive"
  else if x < 0 then "Negative"
  else "Zero"

-- HC3T2 - Grade based on score using guards
grade :: Int -> String
grade x
  | x >= 90 = "A"
  | x >= 80 = "B"
  | x >= 70 = "C"
  | x >= 60 = "D"
  | otherwise = "F"

-- HC3T3 - RGB to Hex using let bindings
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
  let rHex = printf "%02X" r
      gHex = printf "%02X" g
      bHex = printf "%02X" b
  in rHex ++ gHex ++ bHex

-- HC3T4 - Area of triangle using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

-- HC3T5 - Triangle type using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a == b && b == c = "Equilateral"
  | a == b || b == c || a == c = "Isosceles"
  | otherwise = "Scalene"

-- HC3T6 - Check leap year
isLeapYear :: Int -> Bool
isLeapYear year =
  if year `mod` 400 == 0 then True
  else if year `mod` 100 == 0 then False
  else if year `mod` 4 == 0 then True
  else False

-- HC3T7 - Season based on month
season :: Int -> String
season m
  | m == 12 || m == 1 || m == 2 = "Winter"
  | m >= 3 && m <= 5 = "Spring"
  | m >= 6 && m <= 8 = "Summer"
  | m >= 9 && m <= 11 = "Autumn"
  | otherwise = "Invalid month"

-- HC3T8 - BMI with category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25 = "Normal"
  | bmi < 30 = "Overweight"
  | otherwise = "Obese"
  where bmi = weight / (height * height)

-- HC3T9 - Max of three using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z =
  let maxXY = max x y
      maxXYZ = max maxXY z
  in maxXYZ

-- HC3T10 - Palindrome check using recursion and guards
isPalindrome :: String -> Bool
isPalindrome str
  | length str <= 1 = True
  | head str == last str = isPalindrome (init (tail str))
  | otherwise = False

-- Main to run all tests
main :: IO ()
main = do
  putStrLn "--- HC3T1 ---"
  print (checkNumber 5)      -- "Positive"
  print (checkNumber (-3))   -- "Negative"
  print (checkNumber 0)      -- "Zero"

  putStrLn "\n--- HC3T2 ---"
  print (grade 95)           -- "A"
  print (grade 72)           -- "C"
  print (grade 50)           -- "F"

  putStrLn "\n--- HC3T3 ---"
  print (rgbToHex (255, 0, 127))  -- "FF007F"
  print (rgbToHex (0, 255, 64))   -- "00FF40"

  putStrLn "\n--- HC3T4 ---"
  print (triangleArea 3 4 5)      -- 6.0
  print (triangleArea 7 8 9)      -- 26.83 approx

  putStrLn "\n--- HC3T5 ---"
  print (triangleType 3 3 3)      -- "Equilateral"
  print (triangleType 5 5 8)      -- "Isosceles"
  print (triangleType 6 7 8)      -- "Scalene"

  putStrLn "\n--- HC3T6 ---"
  print (isLeapYear 2000)         -- True
  print (isLeapYear 1900)         -- False
  print (isLeapYear 2024)         -- True

  putStrLn "\n--- HC3T7 ---"
  print (season 3)                -- "Spring"
  print (season 7)                -- "Summer"
  print (season 11)               -- "Autumn"

  putStrLn "\n--- HC3T8 ---"
  print (bmiCategory 70 1.75)     -- "Normal"
  print (bmiCategory 90 1.8)      -- "Overweight"

  putStrLn "\n--- HC3T9 ---"
  print (maxOfThree 10 20 15)     -- 20
  print (maxOfThree 5 25 10)      -- 25

  putStrLn "\n--- HC3T10 ---"
  print (isPalindrome "racecar")  -- True
  print (isPalindrome "haskell")  -- False
  print (isPalindrome "madam")    -- True
