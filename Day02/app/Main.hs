{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text (splitOn)
import qualified Data.Text as T
import Debug.Trace (traceM)
import Control.Monad (when)

main :: IO ()
main = do 
   input <- readFile "input"
   let ranges = splitOn "," (T.pack input)
   let parsedRanges = [(read . T.unpack $ start, read . T.unpack $ end) | [start, end] <- map (splitOn "-") ranges]
   let sumInvalid = sum . concat $ findInvalid isValid <$> parsedRanges
   print $ "Part1 - " ++ show sumInvalid
   let sumPart2Invalid = sum . concat $ findInvalid (not . isRepeated) <$> parsedRanges
   print $ "Part2 - " ++ show sumPart2Invalid

findInvalid :: (String -> Bool) -> (Integer, Integer) -> [Integer]
findInvalid isValidF (start, end) = [x | x <- [start..end], not (isValidF . show $ x)]

isValid :: Eq a => [a] -> Bool
isValid n = if even $ length n then 
                let (left, right) = splitAt (length n `div` 2) n 
                in left /= right
            else True

isRepeated :: Eq a => [a] -> Bool
isRepeated what = 
     any (==what) [concat (take (howMany what n) (repeat (take n what))) | n <- [1..(length what) `div` 2]]
    where 
        howMany what n = ceiling (fromIntegral (length what) / fromIntegral n)
