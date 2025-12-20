{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Data.Text as T
import Data.List (zip4, zip5, zipWith4)
import Data.List.Split (splitWhen)

main :: IO ()
main = do 
    input <- lines <$> readFile "input"
    let parsed = words <$> input
    let numbers = fmap (read @Int) <$> take 4 parsed
    let operations = drop 4 parsed !! 0
    let problems = zip5 (numbers !! 0) (numbers !! 1) (numbers !! 2) (numbers !! 3) (operations)
    let result = foldl 
            (\acc (a, b, c, d, op) -> 
                case op of
                    "+" -> acc + (a + b + c + d)
                    "*" -> acc + (a * b * c * d)
                    _   -> error $ "Unknown operation: " ++ op ++ " at " ++ show (a, b, c, d, op)
            ) 0 problems
    print $ "Part 1 - " ++ show result

    -- Part 2 -- 
    let numbers = fmap (read @Int) <$> (splitWhen (==[' ', ' ', ' ', ' ']) $ 
                    zipWith4 (\a b c d -> [a,b,c,d]) 
                        (input !! 0) (input !! 1) (input !! 2) (input !! 3))
    let problems = zip numbers operations
    let result = foldl 
            (\acc (nums, op) -> 
                case op of
                    "+" -> acc + foldl (+) 0 nums
                    "*" -> acc + foldl (*) 1 nums
                    _   -> error $ "Unknown operation: " ++ op ++ " at " ++ show (nums, op)
            ) 0 problems
    print $ "Part 2 - " ++ show result