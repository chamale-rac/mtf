module Main where

import Data.List (delete, elemIndex)
import Data.Maybe (fromMaybe)

-- Type alias for readability
type MTFList = [Int]

type ConfigList = [Int]

-- Function to move an element to the front of the list
moveToFront :: Int -> MTFList -> MTFList
moveToFront x xs = x : delete x xs

-- Type for storing the request list and its index
type RequestInfo = (ConfigList, Int)

-- Modified access function to implement IMTF
accessIMTF :: Int -> MTFList -> RequestInfo -> (MTFList, Int)
accessIMTF x xs (requests, idx) = (newList, cost)
  where
    position = elemIndex x xs
    cost = maybe (length xs + 1) (+ 1) position
    lookAhead = take (fromMaybe 0 position) (drop (idx + 1) requests)
    newList = if x `elem` lookAhead then moveToFront x xs else xs

-- Adjusted processRequests to handle request with index
processRequests :: MTFList -> ConfigList -> (MTFList, [(MTFList, Int, Int)])
processRequests xs requests = foldl process (xs, []) (zip requests [0 ..])
  where
    process (lst, acc) (req, idx) =
      let (newList, cost) = accessIMTF req lst (requests, idx)
       in (newList, acc ++ [(newList, req, cost)])

-- Function to process multiple initial list and request tuples
processMultipleRequests :: [(String, MTFList, ConfigList)] -> IO ()
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
        [ ("6.1", [0, 1, 2, 3, 4], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]), -- Minimum cost (best case)
          ("6.2", [0, 1, 2, 3, 4], [4, 3, 2, 1, 0, 4, 3, 2, 1, 0, 4, 3, 2, 1, 0, 4, 3, 2, 1, 0]) -- Maximum cost (worst case)
        ]
  processMultipleRequests tuples
