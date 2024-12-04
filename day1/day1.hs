-- Day 1: Historian Hysteria

import System.IO
import Control.Monad
import Data.List

----- PART I --------

-- Reading the input
inputData :: IO ([Int], [Int])
inputData = do
  contents <- readFile "day1-input"
  let linesContent = lines contents
      pairs = map (map read . words) linesContent -- Here map read converts each separate string from the file
      (leftList:rightList:_) = transpose pairs    -- into an Int, instead of a string.
  return (leftList, rightList)

-- Finding the nth-smallest pair of both lists
n :: [Int]
n = [0..999]

-- lr :: [Int]
-- lr = [10, 50, 54, 53, 23479, 24, 53, 10, 50, 11, 42, 68]

-- ll :: [Int]
-- ll = [23, 534, 645, 6453, 654, 64, 50, 10, 11, 32, 42, 64]

nMinimum :: [Int] -> Int -> Int
nMinimum xs n = (!!) (sort xs) n

-- It takes two lists to be paired and another list of integers which will provide
-- the index of minimium number to be attained. We leverage here Haskell’s functionality
-- for lazy evaluation and partial application of functions. The helpers pairFirst and pairSecond
-- are defined partially so that they can then be provided with an argument (a Int) which we do
-- by mapping them through each element of the index list.
pairs :: [Int] -> [Int] -> [Int] -> [(Int, Int)]
pairs ps qs ns = zip (map pairFirst ns) (map pairSecond ns)
                     where
                       pairFirst = nMinimum ps
                       pairSecond = nMinimum qs

-- Distance of pairs
pairDist :: (Int, Int) -> Int
pairDist (a, b) = abs (a - b)

-- Sum of all distances
distTotal :: [(Int, Int)] -> Int
distTotal ls = sum (map pairDist ls)

---- PART II ------

-- Count the number of times the same number appears in a list
numberCount :: Int -> [Int] -> Int
numberCount n zs = (length . filter (==n)) zs

-- Similarity score for each number
similarityScore :: [Int] -> Int -> Int
similarityScore qs x = x * (numberCount x qs)

-- Total similarity score for the entire (left) list
totalSimilarity :: [Int] -> [Int] -> Int
totalSimilarity bs cs = sum (map leftSimilarity bs)
                        where
                          leftSimilarity = similarityScore cs

main :: IO ()
main = do
  (leftList, rightList) <- inputData
  let totalDistance = distTotal (pairs leftList rightList n)
  let totalLeftSimilarity = totalSimilarity leftList rightList
  print ("The total distance of between the pairs of the lists are: " ++ show totalDistance)
  print ("The similarity score of the two lists are: " ++ show totalLeftSimilarity)
