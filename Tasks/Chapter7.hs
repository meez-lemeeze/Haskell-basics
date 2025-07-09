import Text.Read (readMaybe)

-- C7T1: Define Color with Eq
data Color = Red | Green | Blue deriving (Show, Read, Bounded, Enum)

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

-- C7T2: Define Ord for Color (Red < Green < Blue)
instance Ord Color where
  compare Red Green   = LT
  compare Red Blue    = LT
  compare Green Blue  = LT
  compare Green Red   = GT
  compare Blue Red    = GT
  compare Blue Green  = GT
  compare _ _         = EQ

-- C7T3: Function using multiple constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues a b = if a >= b then a else b

-- C7T4: Define Shape and implement Show & Read
data Shape = Circle Double | Rectangle Double Double

instance Show Shape where
  show (Circle r)        = "Circle " ++ show r
  show (Rectangle w h)   = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
  readsPrec _ input = 
    case words input of
      ("Circle":r:_)       -> [(Circle (read r), "")]
      ("Rectangle":w:h:_)  -> [(Rectangle (read w) (read h), "")]
      _                    -> []

-- C7T5: squareArea with Num constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- C7T6: circleCircumference with Integral and Floating
circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference r = 2 * pi * realToFrac r

-- C7T7: nextColor using Bounded and Enum
nextColor :: Color -> Color
nextColor color = if color == maxBound then minBound else succ color

-- C7T8: parseShape using Read
parseShape :: String -> Maybe Shape
parseShape str = readMaybe str :: Maybe Shape

-- C7T9: Define Describable type class
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is true."
  describe False = "This is false."

instance Describable Shape where
  describe (Circle r)        = "A circle with radius " ++ show r
  describe (Rectangle w h)   = "A rectangle with width " ++ show w ++ " and height " ++ show h

-- C7T10: describeAndCompare using Describable and Ord
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare a b = describe (compareValues a b)

-- Main function to test everything
main :: IO ()
main = do
  putStrLn "--- C7T1 & C7T2: Eq and Ord for Color ---"
  print (Red == Green)           -- False
  print (compare Red Blue)       -- LT
  print (Red < Blue)             -- True

  putStrLn "\n--- C7T3: compareValues ---"
  print (compareValues 10 20)    -- 20
  print (compareValues 'a' 'z')  -- 'z'

  putStrLn "\n--- C7T4: Shape Show & Read ---"
  let s1 = Circle 3.5
  let s2 = Rectangle 4 5
  print s1
  print s2
  print (read "Circle 3.5" :: Shape)
  print (read "Rectangle 4.0 5.0" :: Shape)

  putStrLn "\n--- C7T5: squareArea ---"
  print (squareArea 5)           -- 25

  putStrLn "\n--- C7T6: circleCircumference ---"
  print (circleCircumference 7 :: Double) -- ~43.98

  putStrLn "\n--- C7T7: nextColor ---"
  print (nextColor Red)          -- Green
  print (nextColor Blue)         -- Red

  putStrLn "\n--- C7T8: parseShape ---"
  print (parseShape "Circle 5.0")           -- Just (Circle 5.0)
  print (parseShape "Rectangle 3.0 4.0")    -- Just (Rectangle 3.0 4.0)
  print (parseShape "Triangle 3.0 4.0")     -- Nothing

  putStrLn "\n--- C7T9: Describable ---"
  print (describe True)          -- "This is true."
  print (describe s1)            -- "A circle with radius 3.5"

  putStrLn "\n--- C7T10: describeAndCompare ---"
  print (describeAndCompare (Rectangle 3 4) (Rectangle 5 6))
