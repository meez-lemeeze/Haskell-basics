{-# LANGUAGE InstanceSigs #-}

module HC18 where

import Data.Char (toLower)

-- HC18T1: mapToLower using fmap
mapToLower :: [Char] -> [Char]
mapToLower = fmap toLower

-- HC18T2: Define a binary tree type and Functor instance
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty
  fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

-- HC18T3: incrementTreeValues adds one to every value in a tree using Functor instance
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

-- HC18T4: mapToBits converts [Bool] to ['1' or '0'] using fmap
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

-- HC18T5: Functor instance for Either, fmap applies only to Right
instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap _ (Left e) = Left e
  fmap f (Right a) = Right (f a)

-- HC18T6: applyToMaybe uses fmap to transform the value inside a Maybe
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

-- HC18T7: fmapTuple applies a function to the second element of a tuple (a,b)
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

-- HC18T8: identityLawCheck verifies the Functor identity law
identityLawCheck :: (Functor f, Eq (f a)) => f a -> Bool
identityLawCheck x = fmap id x == x

-- HC18T9: compositionLawCheck verifies the Functor composition law
compositionLawCheck
  :: (Functor f, Eq (f c))
  => (a -> b) -> (b -> c) -> f a -> Bool
compositionLawCheck f g x = fmap (g . f) x == (fmap g . fmap f) x

-- HC18T10: nestedFmap applies a function to a nested structure using multiple fmap
-- Example: nestedFmap (+1) on Maybe (Either e [Int]) increments every Int inside
nestedFmap :: Functor f => (a -> b) -> f (f a) -> f (f b)
nestedFmap = fmap . fmap
