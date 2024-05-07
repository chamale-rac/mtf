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

-- Function to process multiple initial list and request tuples
processMultipleRequests :: [(MTFList, [Int])] -> IO ()
processMultipleRequests = mapM_ processTuple
  where
    processTuple (initialList, requests) = do
      let (finalList, results) = processRequests initialList requests
      mapM_ printResult results
      let totalCost = sum $ map (\(_, _, cost) -> cost) results
      putStrLn $ "Total Cost of Accesses: " ++ show totalCost
    printResult (lst, req, cost) = putStrLn $ "List: " ++ show lst ++ ", Request: " ++ show req ++ ", Cost: " ++ show cost
    -- Print a divider between each tuple
    printDivider = putStrLn "----------------------------------------"

-- Main function to run the example
main :: IO ()
main = do
  let tuples = [([0, 1, 2, 3, 4], [40, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4]), 
                ([0, 1, 2, 3, 4], [4, 3, 2, 1, 0, 1, 2, 3, 4, 3, 2, 1, 0, 1, 2, 3, 4]),
                ([0, 1, 2, 3, 4], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]), -- Minimum cost
                ([0, 1, 2, 3, 4], [4, 3, 2, 1, 0, 4, 3, 2, 1, 0, 4, 3, 2, 1, 0, 4, 3, 2, 1, 0]), -- Maximum cost
                ([0, 1, 2, 3, 4], [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]),
                ([0, 1, 2, 3, 4], [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3])] 
  processMultipleRequests tuples
