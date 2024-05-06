module Main where

import Data.List (delete, elemIndex)

-- Type alias for readability
type MTFList = [Int]

-- Function to move an element to the front of the list
moveToFront :: Int -> MTFList -> MTFList
moveToFront x xs = x : delete x xs

-- Access an element, updating the list and calculating the cost
access :: Int -> MTFList -> (MTFList, Int)
access x xs = (newList, cost)
  where
    newList = moveToFront x xs
    cost = case elemIndex x xs of
      Just idx -> idx + 1 -- Convert 0-based index to 1-based position
      Nothing -> length xs + 1 -- Cost if element not found

-- Process a sequence of accesses on a configuration list
processRequests :: MTFList -> [Int] -> (MTFList, [(MTFList, Int, Int)])
processRequests xs requests = foldl process (xs, []) requests
  where
    process (lst, acc) req =
      let (newList, cost) = access req lst
       in (newList, acc ++ [(newList, req, cost)])

-- Main function to run the example
main :: IO ()
main = do
  let initialList = [0, 1, 2, 3, 4]
  let requests = [0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4]
  let (finalList, results) = processRequests initialList requests
  mapM_ printResult results
  let totalCost = sum $ map (\(_, _, cost) -> cost) results
  putStrLn $ "Total Cost of Accesses: " ++ show totalCost
  where
    printResult (lst, req, cost) = putStrLn $ "List: " ++ show lst ++ ", Request: " ++ show req ++ ", Cost: " ++ show cost
