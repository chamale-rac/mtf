module Main where

import Data.List (delete, elemIndex)

-- Type alias for readability
type MTFList = [Int]

type RequestList = [Int]

-- Function to move an element to the front of the list
moveToFront :: Int -> MTFList -> MTFList
moveToFront x xs
    | x `elem` xs = x : delete x xs
    | otherwise   = xs

-- Access an element, updating the list and calculating the cost
access :: Int -> MTFList -> (MTFList, Int)
access x xs = (newList, cost)
  where
    newList = moveToFront x xs
    position = elemIndex x xs
    cost = maybe (length xs + 1) (+ 1) position -- inherit Just and Nothing from Maybe

-- Process a sequence of accesses on a configuration list
processRequests :: MTFList -> RequestList -> (MTFList, [(MTFList, Int, Int)])
processRequests xs = foldl process (xs, []) -- request has been eta-reduced cause both are used in the same order
  where
    process (lst, acc) req =
      let (newList, cost) = access req lst
       in (newList, acc ++ [(newList, req, cost)])

-- Function to process multiple initial list and request tuples
processMultipleRequests :: [(String, MTFList, RequestList)] -> IO ()
processMultipleRequests = mapM_ processTuple
  where
    processTuple (name, initialList, requests) = do
      putStrLn $ "Processing " ++ name ++ " with initial list: " ++ show initialList ++ " and requests: " ++ show requests
      let (_, results) = processRequests initialList requests
      mapM_ printResult results
      let totalCost = sum $ map (\(_, _, cost) -> cost) results
      putStrLn $ "Total Cost of Accesses: " ++ show totalCost ++ "\n" ++ "----------------------------------------"
    printResult (lst, req, cost) = putStrLn $ "List: " ++ show lst ++ ", Request: " ++ show req ++ ", Cost: " ++ show cost

-- Main function to run the example
main :: IO ()
main = do
  let tuples =
        [ ("1", [0, 1, 2, 3, 4], [0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4]),
          ("2", [0, 1, 2, 3, 4], [4, 3, 2, 1, 0, 1, 2, 3, 4, 3, 2, 1, 0, 1, 2, 3, 4]),
          ("3", [0, 1, 2, 3, 4], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]), -- Minimum cost (best case)
          ("4", [0, 1, 2, 3, 4], [4, 3, 2, 1, 0, 4, 3, 2, 1, 0, 4, 3, 2, 1, 0, 4, 3, 2, 1, 0]), -- Maximum cost (worst case)
          ("5.1", [0, 1, 2, 3, 4], [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]),
          ("5.2", [0, 1, 2, 3, 4], [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3])
        ]
  processMultipleRequests tuples
