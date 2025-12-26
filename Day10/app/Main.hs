{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function ((&))
import Data.Text (splitOn)
import qualified Data.Text as T
import qualified Data.Map as M
import Prelude hiding (product)

type Lights = M.Map Int Int
type Button = [Int]

data Machine = Machine
  { lights :: Lights,
    buttons :: [Button],
    joltage :: [Int]
  }
  deriving (Eq, Show)

processLights :: String -> [Int]
processLights ('[' : rest) = processLights rest
processLights ('.' : rest) = 0 : processLights rest
processLights ('#' : rest) = 1 : processLights rest
processLights (']' : rest) = []

process :: String -> [Int]
process s = 
    let numbers = T.splitOn "," . T.pack $ take (length s - 2) $ (drop 1 s)
        in read @Int . T.unpack <$> numbers

main :: IO ()
main = do
  lines <- lines <$> readFile "input"
  let input = words <$> lines
  let machines =
        input
          & fmap
            ( \(lights : rest) ->
                let (joltage : buttons) = reverse rest
                 in Machine
                      { lights = M.fromList $ zip [0..] $ processLights lights,
                        buttons = process <$> buttons,
                        joltage = process joltage
                      }
            )
  let result = (\Machine{..} -> tryAllButtons lights buttons) <$> machines
  print $ "Part1 - " <> (show . sum $ (length . snd) <$> result)

pushButton :: Lights -> Button -> Lights 
pushButton lights [] = lights 
pushButton lights b = foldl (\lights k -> M.update flipLight k lights ) lights b
 where
    flipLight 0 = Just 1
    flipLight 1 = Just 0

tryAllButtons :: Lights -> [Button] -> (Bool, [Button])
tryAllButtons ls bs = head $ dropWhile ((==False) . fst) pushingAll     
    where 
        pushingAll = [((==ls) (pushAll buttonPushes), buttonPushes) | y <- [1..], buttonPushes <- combinations y bs]
        pushAll = foldl pushButton initialLights
        initialLights = M.fromList $ (\(i,_) -> (i,0)) <$> M.toList ls

combinations :: Int -> [a] -> [[a]]
combinations 1 [] = []
combinations 1 (x:xs) = [x] : combinations 1 xs 
combinations 0 _ = []
combinations n xs = combinations 1 xs `product` combinations (n-1) xs

product :: [[a]] -> [[a]] -> [[a]]
product xs ys = product' xs ys []
 where 
    product' [] _ _ = []
    product' (x:xs) (y:ys) done = (x ++ y) : product' (x:xs) ys (y:done)
    product' (x:xs) [] done = product' xs done []