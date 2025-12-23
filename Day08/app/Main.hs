{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (nub, partition, sortOn)
import Data.Text (splitOn)
import qualified Data.Text as T

main :: IO ()
main = do
  lines <- lines <$> readFile "input"
  let jbxs = (\(x : y : z : []) -> (x, y, z)) <$> (fmap (read @Float . T.unpack) <$> (splitOn "," . T.pack <$> lines))
  let distances = computeDistance jbxs
  let sorted = fst <$> sortOn snd distances
  let thousandShortest = take 1000 $ sorted
  let connectedGroups = connect thousandShortest []
  let largestThree = length <$> (take 3 $ reverse $ sortOn length connectedGroups)
  print $ "Part1 - " ++ show (foldl (*) 1 largestThree)
  print $ "Part2 - " ++ show (toInteger . round $ connectUntilOne sorted [])

computeDistance [] = []
computeDistance (x : xs) = [((x, y), cartesianDistance3D x y) | y <- xs] ++ computeDistance xs

cartesianDistance3D :: (Floating a) => (a, a, a) -> (a, a, a) -> a
cartesianDistance3D (x1, y1, z1) (x2, y2, z2) =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)

connect [] connectedGroups = connectedGroups
connect ((x, y) : xs) connectedGroups =
  let (withXorY, withoutXorY) = partition (\g -> x `elem` g || y `elem` g) connectedGroups
   in case withXorY of
        [] -> connect xs ([x, y] : withoutXorY)
        _ ->
          let mergedGroup = nub $ x : y : concat withXorY
           in connect xs (mergedGroup : withoutXorY)

connectUntilOne ((x, y) : xs) connectedGroups =
  let (withXorY, withoutXorY) = partition (\g -> x `elem` g || y `elem` g) connectedGroups
   in case withXorY of
        [] -> connectUntilOne xs ([x, y] : withoutXorY)
        _ ->
          let mergedGroup = nub $ x : y : concat withXorY
              newConnectedGroups = mergedGroup : withoutXorY
           in if length newConnectedGroups == 1 && ((==1000) . length . concat $ newConnectedGroups)
              then 
                let (x1,_,_) = x
                    (y1,_,_) = y
                in x1*y1
              else connectUntilOne xs newConnectedGroups