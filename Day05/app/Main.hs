{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Data.Text (splitOn)
import qualified Data.Text as T
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes, isJust)

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
    print $ "Part1 - " ++ show (length freshIngredients)
    let mergedRanges = mergeOverLappling orderedRanges
        sumRanges = sum [end - start + 1 | (start, end) <- mergedRanges]
    print $ "Part2 - " ++ show sumRanges

mergeOverLappling :: [(Int, Int)] -> [(Int, Int)]
mergeOverLappling [] = []
mergeOverLappling (x : xs) =
  let merged i j =
        let ij = overlap i j
            ji = overlap j i
         in if isJust ij then ij else ji
      overlap (i1start, i2end) (j1start, j2end) =
        if j1start <= i2end && j1start >= i1start
          then Just (i1start, max i2end j2end)
          else Nothing

      mergeHelper current [] [] _ = [current]
      mergeHelper current [] (y : ys) False = current : mergeHelper y ys [] False
      mergeHelper current [] remaining True = mergeHelper current remaining [] False
      mergeHelper current (y : ys) remaining mergedSomething =
        case merged current y of
          Just merged -> mergeHelper merged ys remaining True
          Nothing -> mergeHelper current ys (y : remaining) mergedSomething
   
   in mergeHelper x xs [] False