{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Data.Text (splitOn)
import qualified Data.Text as T
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)

main :: IO ()
main = do 
    input <- lines <$> readFile "input"
    let (ranges, ingredients) = 
            foldr (\line (rs, igs) -> 
                if T.null line then (rs, igs) 
                else case splitOn "-" line of
                        [rStart, rEnd] -> ((read @Int $ T.unpack rStart, read @Int $ T.unpack rEnd) : rs, igs)
                        [ingredient] -> (rs, read @Int (T.unpack ingredient) : igs)
            ) ([], []) (T.pack <$> input)
    let orderedIngredients = sort ingredients
        orderedRanges = sortBy (comparing fst) ranges
        ingInRange i (start, end) = i >= start && i <= end
        isFresh = \i -> if any (==True) $ ingInRange i <$> orderedRanges then Just i else Nothing
        freshIngredients = catMaybes $ isFresh <$> orderedIngredients
    print $ length freshIngredients
