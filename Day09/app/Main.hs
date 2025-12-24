{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (sortOn)
import Data.Text (splitOn)
import qualified Data.Text  as T
import Data.List (find)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  lines <- lines <$> readFile "input"
  let reds = (\(y : x : []) -> (x, y)) <$> (fmap (read @Int . T.unpack) <$> (splitOn "," . T.pack <$> lines))
  let rectangles = reverse . sortOn snd $ computeRectangles reds
  let greenBorders = computeBorders (head reds) reds
  let biggestInGreen = find (\r -> rectInsideBorders r greenBorders) (fst <$> rectangles)
  print $ "Part1 - " <> show (snd $ head rectangles)
  print $ "Part2 - " <> (show . snd . fromJust $ (find (\(r, _) -> r == fromJust biggestInGreen) rectangles))

computeRectangles [] = []
computeRectangles (i@(x1, y1) : bricks) = 
    [((i, j), (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)) | j@(x2, y2) <- bricks] 
        ++ computeRectangles bricks

computeBorders first (i : []) = [(i, first)]
computeBorders first (i : j : bricks) = (i, j) : computeBorders first (j : bricks)

type Rectangle = ((Int,Int), (Int,Int))
type Line = ((Int,Int), (Int,Int))

pointInsideRect :: Rectangle -> (Int, Int) -> Bool
pointInsideRect  ((x1,y1), (x2,y2)) (px, py) = 
    case (x1 < x2, y1 < y2)  of 
        (True, True) -> px > x1 && px < x2 && py > y1 && py <y2
        (True, False) -> px > x1 && px < x2 && py < y1 && py > y2
        (False, False) -> px < x1 && px > x2 && py < y1 && py > y2
        (False, True) -> px < x1 && px > x2 && py > y1 && py < y2

intersects :: Rectangle -> Line -> Bool 
intersects r ((lx1, ly1), (lx2, ly2)) = 
    if lx1 == lx2 -- vertical line 
    then 
        any (pointInsideRect r) $ [(lx1,y) | y <- between ly1 ly2]
    else 
        if ly1 == ly2 -- horizontal line
        then any (pointInsideRect r) $ [(x,ly1) | x <- between lx1 lx2]
        else error "Wrong assumption about border lines"
 where 
    between a b = if a > b then [x | x <- [b..a]] else [x | x<-[a..b]] 

rectInsideBorders r = not . any (intersects r)
