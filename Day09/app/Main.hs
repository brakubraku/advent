{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text (splitOn)
import qualified Data.Text as T
import Data.List (sortOn)

main :: IO ()
main = do
  lines <- lines <$> readFile "input"
  let reds = (\(y : x : []) -> (x, y)) <$> (fmap (read @Int . T.unpack) <$> (splitOn "," . T.pack <$> lines))
  let rectangles = reverse . sortOn snd $ computeRectangles reds
  print $ "Part1 - " <> show (snd $ head rectangles)

computeRectangles [] = []
computeRectangles (i@(x1, y1) : bricks) = [((i, j), (abs (x1 - x2) +1) * (abs (y1 - y2)+1)) | j@(x2, y2) <- bricks] ++ computeRectangles bricks
