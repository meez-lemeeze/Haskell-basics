{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HC17 where

import Data.Semigroup
import Data.Monoid

-- HC17T1: Severity data type and Semigroup instance (higher severity overrides lower)
data Severity = Low | Medium | High | Critical
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Semigroup Severity where
  (<>) = max

-- HC17T2: Min and Max newtypes with Semigroup instances using min and max

newtype Min a = Min { getMin :: a }
  deriving (Eq, Show, Ord)

instance Ord a => Semigroup (Min a) where
  Min x <> Min y = Min (min x y)

newtype Max a = Max { getMax :: a }
  deriving (Eq, Show, Ord)

instance Ord a => Semigroup (Max a) where
  Max x <> Max y = Max (max x y)

-- HC17T3: Monoid instance for Severity (identity = Low)
instance Monoid Severity where
  mempty = Low
  mappend = (<>)

-- HC17T4: Monoid instance for Sum newtype
-- Sum is already defined in Data.Monoid, re-implementing for clarity:
newtype SumInt = SumInt { getSumInt :: Int }
  deriving (Eq, Show, Num)

instance Semigroup SumInt where
  SumInt x <> SumInt y = SumInt (x + y)

instance Monoid SumInt where
  mempty = SumInt 0
  mappend = (<>)

-- HC17T5: combineLists concatenates two lists using Semigroup instance
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

-- HC17T6: maxSeverity combines a list of Severity values using mconcat
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

-- HC17T7: multiplyProducts combines a list of Product values using mconcat
-- Product is from Data.Monoid:
import Data.Monoid (Product(..))

multiplyProducts :: [Product Int] -> Product Int
multiplyProducts = mconcat

-- HC17T8: foldWithSemigroup combines a list of any Semigroup type using foldr
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

-- HC17T9: Config data type with Semigroup instance combining loggingLevel, timeout, retries
data Config = Config
  { loggingLevel :: Severity
  , timeout      :: Int     -- timeout in seconds
  , retries      :: Int
  } deriving (Eq, Show)

instance Semigroup Config where
  c1 <> c2 = Config
    { loggingLevel = loggingLevel c1 <> loggingLevel c2  -- max severity
    , timeout      = min (timeout c1) (timeout c2)       -- min timeout
    , retries      = max (retries c1) (retries c2)       -- max retries
    }

-- HC17T10: Monoid instance for Config with identity as:
-- lowest loggingLevel (Low), highest timeout (maxBound :: Int), lowest retries (0)
instance Monoid Config where
  mempty = Config Low maxBound 0
  mappend = (<>)
