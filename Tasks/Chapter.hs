{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Base data types

data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show, Eq, Ord)
data Blockchain = Bitcoin | Ethereum | Cardano deriving (Show)

-- HC10T1: ShowSimple type class and PaymentMethod instance
class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash           = "Cash"
  showSimple Card           = "Card"
  showSimple Cryptocurrency = "Cryptocurrency"

-- HC10T2: Summable type class and Int instance
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

-- HC10T3: Comparable type class and Blockchain instance
class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith Bitcoin Ethereum = LT
  compareWith Ethereum Bitcoin = GT
  compareWith Bitcoin Cardano  = LT
  compareWith Cardano Bitcoin  = GT
  compareWith Ethereum Cardano = LT
  compareWith Cardano Ethereum = GT
  compareWith x y | x == y    = EQ
                  | otherwise = EQ -- fallback for unexpected

instance Eq Blockchain where
  (==) Bitcoin Bitcoin = True
  (==) Ethereum Ethereum = True
  (==) Cardano Cardano = True
  (==) _ _ = False

-- HC10T4: Parameterized Box type and Eq instance
data Box a = EmptyBox | BoxValue a deriving (Show)

instance Eq a => Eq (Box a) where
  EmptyBox == EmptyBox           = True
  (BoxValue x) == (BoxValue y)   = x == y
  _ == _                        = False

-- HC10T5: ShowDetailed type class and User type with instance
class ShowDetailed a where
  showDetailed :: a -> String

data User = User { userName :: String, userAge :: Int }

instance ShowDetailed User where
  showDetailed (User n a) = "User: " ++ n ++ ", Age: " ++ show a

-- HC10T6: Eq for Blockchain with mutual recursion
instance Eq Blockchain where
  x == y = not (x /= y)
  x /= y = not (x == y)

-- HC10T7: Convertible type class, PaymentMethod -> String
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert = showSimple

-- HC10T8: AdvancedEq subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool

instance AdvancedEq PaymentMethod where
  compareEquality = (==)

instance AdvancedEq Blockchain where
  compareEquality = (==)

-- HC10T9: MinMax type class with Int instance
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

-- HC10T10: Concatenatable type class for [Char]
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith = (++)

-- HC11T1: WeAccept type class and Box instance for PaymentMethod
class WeAccept a where
  accept :: a -> Bool

instance WeAccept (Box PaymentMethod) where
  accept (BoxValue Cash)           = True
  accept (BoxValue Card)           = True
  accept (BoxValue Cryptocurrency) = False
  accept EmptyBox                 = False

filterAcceptedBoxes :: [Box PaymentMethod] -> [Box PaymentMethod]
filterAcceptedBoxes = filter accept

-- HC11T2: WeAccept for Cardano, Cash, Country
data CardanoType = CardanoType
data CashType = CashType
data Country = Country String

instance WeAccept CardanoType where accept _ = True
instance WeAccept CashType where accept _ = True
instance WeAccept Country where accept _ = False

fancyFunction :: WeAccept a => a -> String
fancyFunction x = if accept x then "Accepted" else "Rejected"

-- HC11T3: Container type class and Box instance
class Container c a | c -> a where
  isEmpty  :: c -> Bool
  contains :: c -> a -> Bool
  replace  :: c -> a -> c

instance Eq a => Container (Box a) a where
  isEmpty EmptyBox = True
  isEmpty _        = False

  contains EmptyBox _ = False
  contains (BoxValue x) v = x == v

  replace _ v = BoxValue v

-- HC11T4: Container instance for Present type
data Present a = EmptyPresent | PresentValue a deriving Show

instance Eq a => Container (Present a) a where
  isEmpty EmptyPresent = True
  isEmpty _            = False

  contains EmptyPresent _ = False
  contains (PresentValue x) v = x == v

  replace _ v = PresentValue v

-- HC11T5: guessWhat'sInside function
guessWhatsInside :: (Container c a, Eq a) => c -> a -> Bool
guessWhatsInside container item = contains container item

-- HC11T6: AdvancedEq extending Eq for Blockchain
class Eq a => AdvancedEq2 a where
  compareEquality :: a -> a -> Bool

instance AdvancedEq2 Blockchain where
  compareEquality = (==)

-- HC11T7: Ord instance for Box
instance Ord a => Ord (Box a) where
  compare EmptyBox EmptyBox           = EQ
  compare EmptyBox _                  = LT
  compare _ EmptyBox                  = GT
  compare (BoxValue x) (BoxValue y)  = compare x y

-- HC11T8: Eq and Ord already derived for PaymentMethod above

-- HC11T9: Length type with manual Ord for meters and kilometers
data Length = M Float | Km Float deriving (Show, Eq)

instance Ord Length where
  compare (M x) (M y)   = compare x y
  compare (Km x) (Km y) = compare x y
  compare (M x) (Km y)  = compare x (y * 1000)
  compare (Km x) (M y)  = compare (x * 1000) y

-- HC11T10: sortContainers function
sortContainers :: Ord c => [c] -> [c]
sortContainers []     = []
sortContainers (p:xs) = sortContainers [x | x <- xs, x <= p]
                      ++ [p]
                      ++ sortContainers [x | x <- xs, x > p]

-- Test samples (optional)

main :: IO ()
main = do
  putStrLn $ showSimple Cash
  print $ sumUp [1..10 :: Int]
  print $ compareWith Bitcoin Ethereum
  print $ BoxValue 5 == BoxValue 5
  putStrLn $ showDetailed (User "Alice" 28)
  putStrLn $ convert Card
  print $ compareEquality Card Cash
  putStrLn $ concatWith "Hello, " "world!"
  print $ filterAcceptedBoxes [BoxValue Cash, BoxValue Cryptocurrency, EmptyBox]
  putStrLn $ fancyFunction CardanoType
  putStrLn $ fancyFunction (Country "USA")
  print $ guessWhatsInside (BoxValue 5) 5
  print $ compare (BoxValue 10) EmptyBox
  print $ sortContainers [Km 0.5, M 300, Km 0.3, M 100]
