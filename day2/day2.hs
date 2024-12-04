-- Day 2: Red-Nosed Reports

import Control.Monad
import Data.List
import System.IO

----- PART I --------
inputData :: IO ([[Int]])
inputData = do
  contents <- readFile "day2-input"
  let linesConent = lines contents
      reports = map (map read . words) linesConent -- Maps first words to separate the strings by \n, then recursively maps read for each sublist
  return reports

testReport :: [Int]
testReport = [7, 6, 4, 2, 1] -- [69,67,64,63,61,60,58,55]

-- Is the list sorted?
isSortedAsc xs = all (uncurry (<=)) (zip xs (tail xs))
isSortedDesc xs = all (uncurry (>=)) (zip xs (tail xs))

-- Are the adjacent pairs following the condition?
adjacentDiff (a,b) = abs (a - b) >= 1 && abs (a - b) <= 3

testSafety :: [Int] -> Bool
testSafety xs = isSorted xs && adjacentLevels xs
                where
                  isSorted ps = isSortedAsc ps || isSortedDesc ps
                  adjacentLevels qs = all adjacentDiff (zip qs (tail qs))

------ PART II ---------

dropUntilSafe :: ([Int] -> Bool) -> [Int] -> Bool
dropUntilSafe cond xs = go xs 0
  where
    go list deletedCount
      | length list <= 1 = False -- If at the end, our list is singleton or empty, the list is unsafe (False).
      | deletedCount >= length xs = False -- If our count has reached the length of the list, it’s unsafe.
      | cond (removeNth list deletedCount) = True -- If after removing nth element, the list passes condition, its safe.
      | otherwise = go xs (deletedCount + 1) -- else keep applying go to the list and increase the count by one.

    removeNth list n = take n list ++ drop (n+1) list -- Helper function. take n keeps elements before n, and drop (n+1) keeps elements after n

main :: IO ()
main = do
  reports <- inputData
  let safetyReports = map testSafety reports
  let actuallySafetyReports = map (dropUntilSafe testSafety) reports
  let safeReports = filter id safetyReports
  let actuallySafeReports = filter id actuallySafetyReports
  print ("The number of total safe reports are: " ++ show (length safeReports))
  print ("Sorry! We messed up something, the actually safe reports are: " ++ show (length actuallySafeReports))
