{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import Data.Typeable
import Text.Read (readMaybe)
import System.IO (readFile)
import Control.Monad (when)

-- HC15T3: Custom exception for traffic light errors
data TrafficLightException = InvalidLightColor String deriving (Show, Typeable)
instance Exception TrafficLightException

-- HC15T2: Self-driving AI reacting to traffic lights with possible exception
reactToTrafficLight :: String -> IO ()
reactToTrafficLight color = case color of
  "Red"    -> putStrLn "Stop the car."
  "Yellow" -> putStrLn "Slow down."
  "Green"  -> putStrLn "Go."
  _        -> throwIO (InvalidLightColor color)  -- Throw custom exception

-- HC15T4: Handler for traffic light exceptions
handleTrafficLightException :: TrafficLightException -> IO ()
handleTrafficLightException (InvalidLightColor c) = 
  putStrLn $ "Error: Invalid traffic light color '" ++ c ++ "'. Please enter Red, Yellow, or Green."

-- HC15T5: Safe division using Maybe to avoid divide-by-zero
safeDivide :: Fractional a => a -> a -> Maybe a
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- HC15T8: Division with Either providing error messages
safeDivideEither :: (Eq a, Fractional a, Show a) => a -> a -> Either String a
safeDivideEither _ 0 = Left "Error: Division by zero"
safeDivideEither x y = Right (x / y)

-- HC15T6: Safe parsing using readMaybe (Maybe)
safeReadInt :: String -> Maybe Int
safeReadInt = readMaybe

-- HC15T7: Velocity calculation with Maybe and safe parsing
calculateVelocityMaybe :: String -> String -> Maybe Double
calculateVelocityMaybe distStr timeStr = do
  dist <- readMaybe distStr :: Maybe Double
  time <- readMaybe timeStr :: Maybe Double
  safeDivide dist time

-- HC15T1 + HC15T9 + HC15T10: Read file and calculate velocity with hybrid error handling
calculateVelocityFromFile :: FilePath -> IO ()
calculateVelocityFromFile filename = do
  contentOrErr <- try (readFile filename) :: IO (Either IOException String)
  case contentOrErr of
    Left ex -> putStrLn $ "Failed to read file: " ++ show ex
    Right content -> do
      let ls = lines content
      case ls of
        (distStr:timeStr:_) -> case safeDivideEither (read distStr) (read timeStr) of
          Left err -> putStrLn err
          Right velocity -> putStrLn $ "Calculated velocity from file: " ++ show velocity
        _ -> putStrLn "File must contain at least two lines: distance and time."

main :: IO ()
main = do
  -- HC15T1: Read distance and time from user, handle exceptions in velocity calculation
  putStrLn "Enter distance (meters):"
  distStr <- getLine
  putStrLn "Enter time (seconds):"
  timeStr <- getLine
  case calculateVelocityMaybe distStr timeStr of
    Nothing -> putStrLn "Invalid input or division by zero."
    Just velocity -> putStrLn $ "Velocity: " ++ show velocity ++ " m/s"

  -- HC15T2 + HC15T3 + HC15T4: AI car reacting to traffic light with exception handling
  putStrLn "\nEnter traffic light color (Red, Yellow, Green):"
  light <- getLine
  reactToTrafficLight light `catch` handleTrafficLightException

  -- HC15T10: Read velocity from file with hybrid error handling
  putStrLn "\nCalculating velocity from 'velocity.txt' file:"
  calculateVelocityFromFile "velocity.txt"
