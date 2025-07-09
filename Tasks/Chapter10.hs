{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Assume these types from previous context for demonstration:

data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show, Eq, Ord)
data Blockchain = Bitcoin | Ethereum | Cardano deriving (Show, Eq, Ord)

-- HC10T1: ShowSimple type class and instance for PaymentMethod
class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash           = "Cash"
  showSimple Card           = "Card"
  showSimple Cryptocurrency = "Cryptocurrency"

-- HC10T2: Summable type class with sumUp for Int
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

-- HC10T3: Comparable type class with compareWith for Blockchain
class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith = compare  -- reuse Ord instance

-- HC10T4: Box type and Eq instance
data Box a = EmptyBox | BoxValue a deriving (Show)

instance Eq a => Eq (Box a) where
  EmptyBox == EmptyBox         = True
  (BoxValue x) == (BoxValue y) = x == y
  _ == _                      = False

-- HC10T5: ShowDetailed type class and User type
class ShowDetailed a where
  showDetailed :: a -> String

data User = User { userName :: String, userAge :: Int }

instance ShowDetailed User where
  showDetailed (User name age) = "User: " ++ name ++ ", Age: " ++ show age

-- HC10T6: Eq instance for Blockchain using mutual recursion
instance Eq Blockchain where
  x == y = not (x /= y)
  x /= y = not (x == y)

-- HC10T7: Convertible type class and instance for PaymentMethod -> String
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

-- HC10T9: MinMax type class and instance for Int
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

-- HC10T10: Concatenatable type class for lists of Char (Strings)
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith = (++)

-- HC11T1: WeAccept type class and instance for Box
class WeAccept a where
  accept :: a -> Bool

instance WeAccept (Box PaymentMethod) where
  accept (BoxValue Cash) = True
  accept (BoxValue Card) = True
  accept (BoxValue Cryptocurrency) = False
  accept EmptyBox = False

filterAcceptedBoxes :: [Box PaymentMethod] -> [Box PaymentMethod]
filterAcceptedBoxes = filter accept

-- HC11T2: WeAccept type class extended for Cardano, Cash, Country
data Cardano = Cardano
data Cash = Cash
data Country = Country String

instance WeAccept Cardano where accept _ = True
instance WeAccept Cash where accept _ = True
instance WeAccept Country where accept _ = False

fancyFunction :: WeAccept a => a -> String
fancyFunction x = if accept x then "Accepted" else "Rejected"

-- HC11T3: Container type class for Box
class Container c a | c -> a where
  isEmpty :: c -> Bool
  contains :: c -> a -> Bool
  replace :: c -> a -> c

instance Eq a => Container (Box a) a where
  isEmpty EmptyBox   = True
  isEmpty _          = False

  contains EmptyBox _ = False
  contains (BoxValue x) v = x == v

  replace _ v = BoxValue v

-- HC11T4: Container instance for Present
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

-- HC11T6: AdvancedEq extending Eq with compareEquality for Blockchain
class Eq a => AdvancedEq2 a where
  compareEquality :: a -> a -> Bool

instance AdvancedEq2 Blockchain where
  compareEquality = (==)

-- HC11T7: Ord instance for Box using compare function
instance Ord a => Ord (Box a) where
  compare EmptyBox EmptyBox = EQ
  compare EmptyBox _        = LT
  compare _ EmptyBox        = GT
  compare (BoxValue x) (BoxValue y) = compare x y

-- HC11T8: Derive Eq and Ord for PaymentMethod and test
-- (already derived above)

-- HC11T9: Length data type with Eq and manual Ord fix
data Length = M Float | Km Float deriving (Show, Eq)

instance Ord Length where
  compare (M x) (M y)     = compare x y
  compare (Km x) (Km y)   = compare x y
  compare (M x) (Km y)    = compare x (y * 1000)
  compare (Km x) (M y)    = compare (x * 1000) y

-- HC11T10: sortContainers function (sorting list of containers)
sortContainers :: Ord c => [c] -> [c]
sortContainers = quicksort
  where
    quicksort []     = []
    quicksort (p:xs) = quicksort [x | x <- xs, x <= p]
                    ++ [p]
                    ++ quicksort [x | x <- xs, x > p]

-- Example usages:

-- HC10T1 example
pm1 :: PaymentMethod
pm1 = Card
simpleStr :: String
simpleStr = showSimple pm1  -- "Card"

-- HC10T2 example
sumIntList :: Int
sumIntList = sumUp [1..10]  -- 55

-- HC10T3 example
compareBlockchains :: Ordering
compareBlockchains = compareWith Bitcoin Ethereum  -- LT

-- HC10T4 example
box1, box2 :: Box Int
box1 = BoxValue 5
box2 = BoxValue 5
eqBoxes :: Bool
eqBoxes = box1 == box2  -- True

-- HC10T5 example
user1 :: User
user1 = User "Alice" 30
detailedUser :: String
detailedUser = showDetailed user1  -- "User: Alice, Age: 30"

-- HC11T1 example
acceptedBoxes :: [Box PaymentMethod]
acceptedBoxes = filterAcceptedBoxes [BoxValue Cash, BoxValue Cryptocurrency, EmptyBox]

main :: IO ()
main = do
  putStrLn $ "ShowSimple PaymentMethod Card: " ++ simpleStr
  putStrLn $ "Sum of [1..10]: " ++ show sumIntList
  putStrLn $ "Compare Bitcoin and Ethereum: " ++ show compareBlockchains
  putStrLn $ "Boxes equal? " ++ show eqBoxes
  putStrLn $ "Detailed User: " ++ detailedUser
  putStrLn $ "Accepted Boxes: " ++ show acceptedBoxes
  putStrLn $ "Sorted lengths: " ++ show (sortContainers [Km 0.5, M 200, Km 0.3, M 100])
  putStrLn $ "guessWhatsInside box1 (5)? " ++ show (guessWhatsInside box1 5)
  putStrLn $ "fancyFunction Cash: " ++ fancyFunction Cash
  putStrLn $ "fancyFunction Country: " ++ fancyFunction (Country "USA")
