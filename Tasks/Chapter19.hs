{-# LANGUAGE InstanceSigs #-}

module HC19 where

import Control.Applicative
import Control.Monad (forever, when, replicateM)
import Data.Either (Either(..))

-- HC19T1: Applicative instance for Pair a
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure :: a -> Pair a
  pure x = Pair x x

  (<*>) :: Pair (a -> b) -> Pair a -> Pair b
  (Pair f1 f2) <*> (Pair x1 x2) = Pair (f1 x1) (f2 x2)

-- HC19T2: Add three Maybe Int values using applicative style
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative x y z = (+) <$> x <*> y >>= \sumXY -> (+) sumXY <$> z
-- Alternative using liftA3
-- import Control.Applicative (liftA3)
-- addThreeApplicative x y z = liftA3 (\a b c -> a + b + c) x y z

-- HC19T3: safeProduct for list of Maybe Int, returns Nothing if any is Nothing
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = foldr (liftA2 (*)) (Just 1)

-- HC19T4: liftAndMultiply lifts a binary function with liftA2
liftAndMultiply :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiply = liftA2

-- HC19T5: applyEffects takes (IO Int, IO Int), sums values while printing them
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (ioX, ioY) = (+) <$> ioX <*> ioY

-- HC19T6: repeatEffect repeatedly executes an effect using forever
repeatEffect :: IO a -> IO b
repeatEffect action = forever (action >> return undefined) -- returns bottom, or use () as b
-- Better: Just run forever action ignoring return:
-- repeatEffect :: IO a -> IO ()
-- repeatEffect = forever

-- HC19T7: conditionalPrint prints message only if condition is True using when
conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond (putStrLn msg)

-- HC19T8: discardSecond returns first argument after sequencing effects using <*
discardSecond :: Applicative f => f a -> f b -> f a
discardSecond = (<*)

-- HC19T9: pureAndApply demonstrates pure with applicative effects
pureAndApply :: Applicative f => a -> f (a -> b) -> f b
pureAndApply x f = pure x <*> f

-- HC19T10: combineResults combines two Either values applicatively
combineResults :: Either e a -> Either e b -> Either e (a, b)
combineResults = liftA2 (,)

-- HC11T1: Applicative instance for Wrapper a
newtype Wrapper a = Wrapper a deriving (Show, Eq)

instance Functor Wrapper where
  fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
  pure = Wrapper
  (Wrapper f) <*> (Wrapper x) = Wrapper (f x)

-- HC11T2: sumThreeApplicative adds three Either String Int values applicatively
sumThreeApplicative :: Either String Int -> Either String Int -> Either String Int -> Either String Int
sumThreeApplicative x y z = liftA3 (\a b c -> a + b + c) x y z
  where liftA3 f a b c = f <$> a <*> b <*> c

-- HC12T1: whenApplicative executes action if Bool is True
whenApplicative :: Applicative f => Bool -> f () -> f ()
whenApplicative True action = action
whenApplicative False _ = pure ()

-- HC12T2: replicateEffect replicates an IO action n times using replicateM
replicateEffect :: Int -> IO a -> IO [a]
replicateEffect = replicateM

-- HC13T1: sequenceEffects combines list of applicative effects into one
sequenceEffects :: Applicative f => [f a] -> f [a]
sequenceEffects = sequenceA

-- HC14T1: applyWithEffects demonstrates <*> sequencing two effects
applyWithEffects :: IO (a -> b) -> IO a -> IO b
applyWithEffects = (<*>)

-- HC15T1: simulateMaybeEffect applies function to multiple Maybe values applicatively
simulateMaybeEffect :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
simulateMaybeEffect = liftA2

-- HC16T1: combineEitherResults handles multiple Either computations with errors
combineEitherResults :: Either e a -> Either e b -> Either e (a, b)
combineEitherResults = liftA2 (,)

-- HC17T1: sequenceApplicative combines list of Maybe values into Maybe [a]
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

-- HC18T1: replicateForever applies an IO action infinitely using forever
replicateForever :: IO a -> IO b
replicateForever = forever
