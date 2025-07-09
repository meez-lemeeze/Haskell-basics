{-# LANGUAGE FlexibleContexts #-}

module HC20 where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Identity
import System.Random
import System.IO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (toLower)
import Data.Either (Either(..))

-- HC20T1: safeDivide using Maybe monad
safeDivide :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- HC20T2: sequenceMaybe turns [Maybe a] into Maybe [a]
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequence

-- HC20T3: logging calculator using Writer monad
type Log = [String]
type Logger a = Writer Log a

add :: Int -> Int -> Logger Int
add x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

subtract' :: Int -> Int -> Logger Int
subtract' x y = writer (x - y, ["Subtracted " ++ show y ++ " from " ++ show x])

multiply :: Int -> Int -> Logger Int
multiply x y = writer (x * y, ["Multiplied " ++ show x ++ " and " ++ show y])

-- HC20T4: countChars counts occurrences of a char in a string using State monad
countChars :: Char -> String -> Int
countChars c str = execState (mapM_ go str) 0
  where
    go ch = when (ch == c) $ modify (+1)

-- HC20T5: Reader monad greeting system
type Config = String  -- greeting prefix

greet :: Reader Config String
greet = do
  prefix <- ask
  return (prefix ++ ", welcome!")

greetUser :: String -> Reader Config String
greetUser name = do
  prefix <- ask
  return (prefix ++ ", " ++ name ++ "!")

-- HC20T6: doubleMonad combines Maybe and List monads
doubleMonad :: Maybe [a] -> [Maybe a]
doubleMonad Nothing = []
doubleMonad (Just xs) = map Just xs

-- HC20T7: findFirst uses Either to handle errors finding element
findFirst :: Eq a => a -> [a] -> Either String a
findFirst x xs = case filter (== x) xs of
  [] -> Left ("Element " ++ show x ++ " not found")
  (y:_) -> Right y

-- HC20T8: Parser monad for simple expressions (basic)
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (a, rest) <- runParser p input
    return (f a, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)
  pf <*> pa = Parser $ \input -> do
    (f, rest1) <- runParser pf input
    (a, rest2) <- runParser pa rest1
    return (f a, rest2)

instance Monad Parser where
  pa >>= f = Parser $ \input -> do
    (a, rest1) <- runParser pa input
    runParser (f a) rest1

-- parse a single digit
digit :: Parser Char
digit = Parser f
  where
    f (x:xs) | x >= '0' && x <= '9' = Just (x, xs)
    f _ = Nothing

-- parse an integer (single digit only for simplicity)
parseInt :: Parser Int
parseInt = fmap (read . (:[])) digit

-- HC20T9: replicateMonad with Identity
replicateMonad :: Monad m => Int -> m a -> m [a]
replicateMonad n ma = replicateM n ma

-- HC20T10: nested monad transformer StateT + MaybeT
type StateMaybe s a = StateT s (MaybeT Identity) a

runStateMaybe :: StateMaybe s a -> s -> Maybe (a, s)
runStateMaybe st s = runIdentity (runMaybeT (runStateT st s))

incrementState :: StateMaybe Int Int
incrementState = do
  x <- get
  if x >= 10 then lift $ MaybeT $ return Nothing
  else put (x+1) >> return (x+1)

-- HC20T11: randomWalk using State monad on 2D grid
type Position = (Int, Int)

randomWalk :: State StdGen Position
randomWalk = do
  (x, y) <- getPosition
  gen <- get
  let (dx, gen') = randomR (-1,1) gen
      (dy, gen'') = randomR (-1,1) gen'
  put gen''
  let newPos = (x + dx, y + dy)
  putPosition newPos
  return newPos
  where
    getPosition = return (0,0) -- could use State position if extended
    putPosition pos = return () -- placeholder

-- HC20T12: IO monad reads file and prints lines
printFileLines :: FilePath -> IO ()
printFileLines path = do
  contents <- readFile path
  mapM_ putStrLn (lines contents)

-- HC20T13: fibonacciMemo uses State for memoization
fibonacciMemo :: Int -> State (Map Int Int) Int
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
  memo <- get
  case Map.lookup n memo of
    Just v -> return v
    Nothing -> do
      a <- fibonacciMemo (n-1)
      b <- fibonacciMemo (n-2)
      let v = a + b
      modify (Map.insert n v)
      return v

-- HC20T14: mapMFilter maps and filters elements using a monad
mapMFilter :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMFilter f = fmap catMaybes . mapM f
  where catMaybes = foldr (\x acc -> maybe acc (:acc) x) []

-- HC20T15: treeSum sums elements in a binary tree using a custom monad (State)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

treeSum :: Num a => Tree a -> a
treeSum t = evalState (go t) 0
  where
    go Empty = get
    go (Node v l r) = do
      s <- get
      put (s + v)
      go l
      go r

-- HC20T16: retryIO retries IO operation n times
retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO n action = do
  r <- try action
  case r of
    Right val -> return (Just val)
    Left (_ :: IOError) -> retryIO (n-1) action
  where
    try = tryIOError

-- HC20T17: validatePassword with Either for multiple checks
validatePassword :: String -> Either [String] String
validatePassword pwd = case errs of
  [] -> Right pwd
  _ -> Left errs
  where
    errs = concat [lengthCheck, uppercaseCheck, digitCheck]
    lengthCheck = [ "Password too short" | length pwd < 8]
    uppercaseCheck = [ "No uppercase letter" | not (any (`elem` ['A'..'Z']) pwd)]
    digitCheck = [ "No digit" | not (any (`elem` ['0'..'9']) pwd)]

-- HC20T18: MaybeT monad transformer to combine Maybe and IO for validation
getValidatedInput :: IO (Maybe String)
getValidatedInput = runMaybeT $ do
  lift $ putStrLn "Enter input (non-empty):"
  input <- lift getLine
  guard (not $ null input)
  return input

-- HC20T19: Writer monad logging system tracking function calls and args
logCall :: Show a => String -> a -> Writer [String] ()
logCall fname arg = tell [fname ++ " called with " ++ show arg]

loggedAdd :: Int -> Int -> Writer [String] Int
loggedAdd x y = do
  logCall "loggedAdd" (x,y)
  return (x + y)

-- HC20T20: batchProcessing chains multiple monadic actions using bind
batchProcessing :: Monad m => [m a] -> m [a]
batchProcessing [] = return []
batchProcessing (x:xs) = x >>= \r -> batchProcessing xs >>= \rs -> return (r:rs)
-- alternatively batchProcessing = sequence

