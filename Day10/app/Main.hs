{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Function ((&))
import Data.Text (splitOn)
import qualified Data.Text as T
import qualified Data.Map as M
import Prelude hiding (product)
import Data.Maybe
import Data.Either
import Data.Ord
import Data.List 
import Control.Monad
import Data.Functor

import ILP

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
                        buttons = process <$> (reverse buttons),
                        joltage = process joltage
                      }
            )
  let result1 = (\Machine{..} -> tryAllButtons lights buttons) <$> machines
  print $ "Part1 - " <> (show . sum $ result1)
  result2 <- mapM solve machines
  print $ "Part2 - " <> (show . sum $ result2)

pushButton :: Lights -> Button -> Lights 
pushButton lights [] = lights 
pushButton lights b = foldl (\lights k -> M.update flipLight k lights ) lights b
 where
    flipLight 0 = Just 1
    flipLight 1 = Just 0

tryAllButtons :: M.Map Int Int -> [Button] -> Int
tryAllButtons lights bs = length . head $ 
      dropWhile 
       (\bs -> pushAll bs /= lights)
       (sortOn length $ subsequences bs)
    where 
        pushAll = foldl pushButton initial
        initial = M.fromList $ (\(i,_) -> (i,0)) <$> M.toList lights

solve Machine{..} = solveILP buttons joltage

