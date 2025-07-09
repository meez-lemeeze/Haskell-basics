-- HC9T1: Parametric Type Synonym
type Entity a = (String, a)  -- (name, address of type a)

-- HC9T2: Parametric Data Type Box
data Box a = Empty | Has a deriving (Show, Eq)

-- HC9T3: Add a number to Box value (assuming Num a)
addN :: Num a => a -> Box a -> Box a
addN n Empty   = Empty
addN n (Has x) = Has (x + n)

-- HC9T4: Extract value from Box or return default
extract :: a -> Box a -> a
extract def Empty   = def
extract _   (Has x) = x

-- HC9T5: Parametric data type Shape with record syntax
data Shape a
  = Circle { color :: a, radius :: Float }
  | Rectangle { color :: a, width :: Float, height :: Float }
  deriving Show

-- HC9T6: Recursive Tweet data type
data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet]
  } deriving Show

-- HC9T7: Engagement function sums likes + engagement of comments recursively
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence data type
data Sequence a = Node a (Sequence a) | End deriving Show

-- HC9T9: Check if element is in Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq e (Node x next)
  | e == x    = True
  | otherwise = elemSeq e next

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyNode | NodeBST a (BST a) (BST a) deriving Show

-- Optional: Insert into BST for completeness
bstInsert :: (Ord a) => a -> BST a -> BST a
bstInsert x EmptyNode = NodeBST x EmptyNode EmptyNode
bstInsert x (NodeBST val left right)
  | x == val = NodeBST val left right  -- ignore duplicates
  | x < val  = NodeBST val (bstInsert x left) right
  | x > val  = NodeBST val left (bstInsert x right)

-- Optional: Search in BST
bstSearch :: (Ord a) => a -> BST a -> Bool
bstSearch _ EmptyNode = False
bstSearch x (NodeBST val left right)
  | x == val = True
  | x < val  = bstSearch x left
  | otherwise = bstSearch x right

----------------------------
-- Example usage:

exampleEntity :: Entity String
exampleEntity = ("Home", "123 Main St")

exampleBox1 :: Box Int
exampleBox1 = Has 10

exampleBox2 :: Box Int
exampleBox2 = Empty

exampleAddN :: Box Int
exampleAddN = addN 5 exampleBox1 -- Should be Has 15

exampleExtract1 :: Int
exampleExtract1 = extract 0 exampleBox2 -- Should be 0

exampleShape1 :: Shape String
exampleShape1 = Circle "Red" 10.0

exampleShape2 :: Shape String
exampleShape2 = Rectangle "Blue" 5.0 8.0

exampleTweet :: Tweet
exampleTweet = Tweet "Hello" 10
              [ Tweet "Reply 1" 3 []
              , Tweet "Reply 2" 2 [Tweet "Nested reply" 1 []]
              ]

exampleEngagement :: Int
exampleEngagement = engagement exampleTweet -- Should be 16 (10 + 3 + 2 + 1)

exampleSeq :: Sequence Int
exampleSeq = Node 1 (Node 2 (Node 3 End))

exampleElem1 :: Bool
exampleElem1 = elemSeq 2 exampleSeq -- True

exampleElem2 :: Bool
exampleElem2 = elemSeq 5 exampleSeq -- False

exampleBST :: BST Int
exampleBST = bstInsert 5 (bstInsert 3 (bstInsert 7 EmptyNode))

exampleSearch1 :: Bool
exampleSearch1 = bstSearch 3 exampleBST -- True

exampleSearch2 :: Bool
exampleSearch2 = bstSearch 10 exampleBST -- False

main :: IO ()
main = do
  print exampleEntity
  print exampleBox1
  print exampleAddN
  print exampleExtract1
  print exampleShape1
  print exampleShape2
  print exampleTweet
  putStrLn $ "Engagement: " ++ show exampleEngagement
  print exampleSeq
  putStrLn $ "2 in seq? " ++ show exampleElem1
  putStrLn $ "5 in seq? " ++ show exampleElem2
  print exampleBST
  putStrLn $ "3 in BST? " ++ show exampleSearch1
  putStrLn $ "10 in BST? " ++ show exampleSearch2
