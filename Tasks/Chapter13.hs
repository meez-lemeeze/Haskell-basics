{-# LANGUAGE ScopedTypeVariables #-}

-- HC13 All-in-One Program

module Main where

import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------
-- HC13T1: List all files in current directory
listFiles :: IO [FilePath]
listFiles = listDirectory "."

hc13t1 :: IO ()
hc13t1 = do
  putStrLn "HC13T1: Files in current directory:"
  files <- listFiles
  mapM_ putStrLn files
  putStrLn ""

--------------------------------
-- HC13T2: Filter files by substring
filterFilesBySubstring :: String -> IO [FilePath]
filterFilesBySubstring substr = do
  files <- listFiles
  return $ filter (isInfixOf substr) files

hc13t2 :: IO ()
hc13t2 = do
  putStrLn "HC13T2: Files containing 'hs':"
  filtered <- filterFilesBySubstring "hs"
  mapM_ putStrLn filtered
  putStrLn ""

--------------------------------
-- HC13T3: Sort and return filtered filenames
sortedFilteredFiles :: String -> IO [FilePath]
sortedFilteredFiles substr = do
  files <- listFiles
  return $ sort $ filter (isInfixOf substr) files

hc13t3 :: IO ()
hc13t3 = do
  putStrLn "HC13T3: Sorted files containing 'hs':"
  filteredSorted <- sortedFilteredFiles "hs"
  mapM_ putStrLn filteredSorted
  putStrLn ""

--------------------------------
-- HC13T4 & HC13T5: SumNonEmpty module functionality embedded here
sumNonEmpty :: Num a => [a] -> a
sumNonEmpty [] = error "sumNonEmpty: empty list"
sumNonEmpty xs = sum xs

hc13t4t5 :: IO ()
hc13t4t5 = do
  putStrLn "HC13T4 & HC13T5: sumNonEmpty test"
  print $ sumNonEmpty [1,2,3,4]
  -- Uncomment next line to see error on empty list
  -- print $ sumNonEmpty []
  putStrLn ""

--------------------------------
-- HC13T6: Convert filtered files to a Map
filesToMap :: String -> IO (Map.Map Int FilePath)
filesToMap substr = do
  files <- listFiles
  let filtered = filter (isInfixOf substr) files
      indexed = zip [1..] filtered
  return $ Map.fromList indexed

hc13t6 :: IO ()
hc13t6 = do
  putStrLn "HC13T6: Map of filtered files (index -> filename):"
  m <- filesToMap "hs"
  mapM_ print (Map.toList m)
  putStrLn ""

--------------------------------
-- HC13T7: Use sumNonEmpty
hc13t7 :: IO ()
hc13t7 = do
  putStrLn "HC13T7: sumNonEmpty usage"
  let nums = [10, 20, 30]
  putStrLn $ "Sum of list: " ++ show (sumNonEmpty nums)
  putStrLn ""

--------------------------------
-- HC13T8: Qualified imports to handle conflicts
hc13t8 :: IO ()
hc13t8 = do
  putStrLn "HC13T8: Qualified imports demo"
  let m = Map.fromList [(1,"one"), (2,"two")]
      s = Set.fromList [1,2,3]
  putStrLn $ "Map keys: " ++ show (Map.keys m)
  putStrLn $ "Set elements: " ++ show (Set.toList s)
  putStrLn ""

--------------------------------
-- HC13T9: Rename modules and use
hc13t9 :: IO ()
hc13t9 = do
  putStrLn "HC13T9: Renamed modules demo"
  let myMap = Map.fromList [(1,"a"), (2,"b")]
      mySet = Set.fromList [1,2,3]
  putStrLn $ "Map keys: " ++ show (Map.keys myMap)
  putStrLn $ "Set elements: " ++ show (Set.toList mySet)
  putStrLn ""

--------------------------------
-- HC13T10: Main - search and display sorted files by substring
hc13t10 :: IO ()
hc13t10 = do
  putStrLn "HC13T10: Enter substring to search for:"
  substr <- getLine
  files <- listFiles
  let filtered = filter (isInfixOf substr) files
      sortedFiltered = sort filtered
  putStrLn "Matching files:"
  mapM_ putStrLn sortedFiltered
  putStrLn ""

--------------------------------
-- Main running all tasks in sequence

main :: IO ()
main = do
  hc13t1
  hc13t2
  hc13t3
  hc13t4t5
  hc13t6
  hc13t7
  hc13t8
  hc13t9
  hc13t10
