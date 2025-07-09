-- HC8T1: Type synonyms and generateTx function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr val = 
  "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show val

-- HC8T2: PaymentMethod and Person with tuple address
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person
  { personName :: String
  , personAddress :: (String, Int) -- tuple (street, number)
  , paymentMethod :: PaymentMethod
  } deriving Show

bob :: Person
bob = Person "Bob" ("123 Elm St", 101) Cash

-- HC8T3: Shape data type and area function
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- Example calculations
circleAreaExample :: Float
circleAreaExample = area (Circle 5)

rectangleAreaExample :: Float
rectangleAreaExample = area (Rectangle 10 5)

-- HC8T4: Employee record syntax and instance richard
data Employee = Employee
  { empName :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee { empName = "Richard", experienceInYears = 7.5 }

-- HC8T5: Person record with employed flag and examples
data PersonRec = PersonRec
  { name :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: PersonRec
person1 = PersonRec "Alice" 30 True

person2 :: PersonRec
person2 = PersonRec "John" 25 False

-- HC8T6: Shape record syntax for Circle and Rectangle
data ShapeRec 
  = CircleRec { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleRec { width :: Float, height :: Float, color :: String }
  deriving Show

circleRecExample :: ShapeRec
circleRecExample = CircleRec (0,0) "Red" 10

rectangleRecExample :: ShapeRec
rectangleRecExample = RectangleRec 5 8 "Blue"

-- HC8T7: Animal data type and describeAnimal function
data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "Dog named " ++ name
describeAnimal (Cat name) = "Cat named " ++ name

dogExample :: Animal
dogExample = Dog "Rex"

catExample :: Animal
catExample = Cat "Whiskers"

-- HC8T8: Name and Age type synonyms and greet function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, " ++ n ++ "! You are " ++ show a ++ " years old."

-- HC8T9: Transaction type and createTransaction function
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction f t val = transactionId tx
  where
    tx = Transaction f t val ("tx-" ++ show (length f + length t + val)) -- simple id

-- HC8T10: Book type with Show deriving and example
data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving Show

bookExample :: Book
bookExample = Book "1984" "George Orwell" 1949

-- Main function demonstrating usage
main :: IO ()
main = do
  putStrLn "--- HC8T1 ---"
  putStrLn $ generateTx "AliceAddr" "BobAddr" 1000

  putStrLn "\n--- HC8T2 ---"
  print bob

  putStrLn "\n--- HC8T3 ---"
  print circleAreaExample
  print rectangleAreaExample

  putStrLn "\n--- HC8T4 ---"
  print richard

  putStrLn "\n--- HC8T5 ---"
  print person1
  print person2

  putStrLn "\n--- HC8T6 ---"
  print circleRecExample
  print rectangleRecExample

  putStrLn "\n--- HC8T7 ---"
  putStrLn $ describeAnimal dogExample
  putStrLn $ describeAnimal catExample

  putStrLn "\n--- HC8T8 ---"
  putStrLn $ greet "Charlie" 28

  putStrLn "\n--- HC8T9 ---"
  putStrLn $ createTransaction "Addr1" "Addr2" 500

  putStrLn "\n--- HC8T10 ---"
  print bookExample
