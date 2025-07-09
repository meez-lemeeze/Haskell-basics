-- HC4T1 - Weather Report using Pattern Matching
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2 - Day Type using Pattern Matching
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
  | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
  | otherwise = "Invalid day"

-- HC4T3 - Grade Comment using Guards
gradeComment :: Int -> String
gradeComment g
  | g >= 90 && g <= 100 = "Excellent!"
  | g >= 70 && g <= 89  = "Good job!"
  | g >= 50 && g <= 69  = "You passed."
  | g >= 0  && g <= 49  = "Better luck next time."
  | otherwise           = "Invalid grade"

-- HC4T4 + HC4T5 - Special Birthday with Pattern Matching + Catch-All
specialBirthday :: Int -> String
specialBirthday 1  = "Happy 1st Birthday!"
specialBirthday 18 = "Congrats on adulthood!"
specialBirthday 50 = "Half a century old!"
specialBirthday age = "Happy Birthday! You are " ++ show age ++ " years old."

-- HC4T6 - Describe List Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [_]      = "The list has one element."
whatsInsideThisList [_, _]   = "The list has two elements."
whatsInsideThisList _        = "The list has many elements."

-- HC4T7 - First and Third Element Extraction
firstAndThird :: [a] -> Maybe (a, a)
firstAndThird (x:_:z:_) = Just (x, z)
firstAndThird _         = Nothing

-- HC4T8 - Describe Tuple Contents
describeTuple :: (String, Int) -> String
describeTuple (name, score) = name ++ " scored " ++ show score ++ " points."

-- Main Function to Run All Tests
main :: IO ()
main = do
  putStrLn "\n--- HC4T1: Weather Report ---"
  print (weatherReport "sunny")
  print (weatherReport "rainy")
  print (weatherReport "cloudy")
  print (weatherReport "foggy")

  putStrLn "\n--- HC4T2: Day Type ---"
  print (dayType "Monday")
  print (dayType "Sunday")
  print (dayType "Funday")

  putStrLn "\n--- HC4T3: Grade Comment ---"
  print (gradeComment 95)
  print (gradeComment 75)
  print (gradeComment 55)
  print (gradeComment 30)
  print (gradeComment 120)

  putStrLn "\n--- HC4T4/5: Special Birthday ---"
  print (specialBirthday 1)
  print (specialBirthday 18)
  print (specialBirthday 50)
  print (specialBirthday 21)

  putStrLn "\n--- HC4T6: What's Inside the List ---"
  print (whatsInsideThisList ([] :: [Int]))
  print (whatsInsideThisList [1])
  print (whatsInsideThisList [1, 2])
  print (whatsInsideThisList [1, 2, 3])

  putStrLn "\n--- HC4T7: First and Third Elements ---"
  print (firstAndThird [10, 20, 30, 40])  -- Just (10, 30)
  print (firstAndThird [5])              -- Nothing
  print (firstAndThird [5, 6])           -- Nothing
  print (firstAndThird [1, 2, 3])        -- Just (1, 3)

  putStrLn "\n--- HC4T8: Describe Tuple ---"
  print (describeTuple ("Alice", 90))
  print (describeTuple ("Bob", 75))
