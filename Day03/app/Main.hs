{-# LANGUAGE TypeApplications #-}
module Main where
import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))
import Debug.Trace (trace)

main :: IO ()
main = do 
    banksStrings <- lines <$> readFile "input"
    let banks = [zip [1..length bank] (read @Int <$> ((\c -> [c]) <$> bank)) | bank <- banksStrings]
    let maximums = findMaxExtended 2 <$> banks
    print $ "Part1 - " <> show (sum $ read @Int <$> maximums)
    let extendedMaximums = findMaxExtended 12 <$> banks
    print $ "Part2 - " <> show (sum $ read @Int <$> extendedMaximums)

-- findMax :: [(Int, Int)] -> Int
-- findMax bank = 
--     let sorted = sortBy (comparing (Down . snd)) $ take (length bank -1) bank
--         highest = snd $ head sorted
--         allHighest = filter (\(_, v) -> v == highest) sorted
--         first = head $ sortBy (comparing fst) allHighest
--         allWithHigherIndex = filter (\(i, _) -> i > fst first) bank
--         second = head $ sortBy (comparing (Down . snd)) allWithHigherIndex
--     in snd first * 10 + snd second

findMaxExtended :: Int -> [(Int, Int)] -> String
findMaxExtended 0 _ = []
findMaxExtended noOfDigits bank = 
    -- trace ("Finding max extended for digits: " ++ show noOfDigits ++ " in bankIdx: " ++ show bankIdx ++ " and bank: " ++ show bank) $
    let bankLength = 100
        possibilities = filter (\(ix, _) -> ix <= bankLength - noOfDigits + 1) $ sortBy (comparing (Down . snd)) bank
        highest = head possibilities
    in show (snd highest) ++ findMaxExtended (noOfDigits - 1) (filter (\(i, _) -> i > fst highest) bank)