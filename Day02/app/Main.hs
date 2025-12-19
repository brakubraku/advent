{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text (splitOn)
import qualified Data.Text as T

main :: IO ()
main = do 
   input <- readFile "input"
   let ranges = splitOn ","  (T.pack input)
   let parsedRanges = [(read . T.unpack $ start, read . T.unpack $ end) | [start, end] <- map (splitOn "-") ranges]
   let sumIvalid = sum . concat $ findInvalid <$> parsedRanges
   print sumIvalid

findInvalid :: (Integer, Integer) -> [Integer]
findInvalid (start, end) = [x | x <- [start..end], not (isValid . show $ x)]
 where 
   isValid n = if even $ length n then 
                 let (left, right) = splitAt (length n `div` 2) n 
                 in left /= right
               else True

