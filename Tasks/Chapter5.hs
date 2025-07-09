import Data.Char (isUpper)

-- HC5T1 - Apply function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- HC5T2 - Filter odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- HC5T3 - Check if any word starts with an uppercase letter
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\word -> not (null word) && isUpper (head word))

-- HC5T4 - Rewrite using lambda
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5 - Partial application
multiplyByFive :: Int -> Int
multiplyByFive = (* 5)

-- HC5T6 - Function composition to square numbers and keep evens
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- HC5T7 - Use $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8 - Point-free style
addFive :: Int -> Int
addFive = (+ 5)

-- HC5T9 - Apply a function twice to each element of a list
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10 - Check if any squared number is greater than 50
anySquareOver50 :: [Int] -> Bool
anySquareOver50 = any (>50) . map (^2)

-- Main function to test all the above
main :: IO ()
main = do
  putStrLn "--- HC5T1: applyThrice ---"
  print (applyThrice (+1) 5)        -- 8
  print (applyThrice (*2) 1)        -- 8

  putStrLn "\n--- HC5T2: oddNumbers ---"
  print oddNumbers

  putStrLn "\n--- HC5T3: hasUppercaseStart ---"
  print (hasUppercaseStart ["hello", "world"])   -- False
  print (hasUppercaseStart ["hello", "World"])   -- True

  putStrLn "\n--- HC5T4: biggerThan10 ---"
  print (biggerThan10 8)   -- False
  print (biggerThan10 15)  -- True

  putStrLn "\n--- HC5T5: multiplyByFive ---"
  print (multiplyByFive 7)   -- 35

  putStrLn "\n--- HC5T6: evenSquares ---"
  print (evenSquares [1..10])  -- [4,16,36,64,100]

  putStrLn "\n--- HC5T7: result with $ operator ---"
  print result  -- 92

  putStrLn "\n--- HC5T8: addFive point-free ---"
  print (addFive 10)  -- 15

  putStrLn "\n--- HC5T9: transformList ---"
  print (transformList (+1) [1,2,3])  -- [3,4,5]

  putStrLn "\n--- HC5T10: anySquareOver50 ---"
  print (anySquareOver50 [1,2,3,5,8])  -- True
  print (anySquareOver50 [1,2,3,4])    -- False
