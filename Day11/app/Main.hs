{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Data.Function ((&))
import System.IO.Unsafe

import Data.IORef 

main :: IO ()
main = do 
  lines <- lines <$> readFile "input"
  let input = words <$> lines
  let cs = 
       M.fromList $ 
         input & 
           fmap (\(from:to) -> 
                    (fromJust . T.stripSuffix ":" . T.pack $ from, 
                     T.pack <$> to))
  
  print $ "Part1 - " <> show (length $ findPaths cs "you" "out")
  -- print . length $ findPaths cs "dac" "fft"  -- 0
  -- print . length $ findPaths cs "fft" "dac"  -- 4605696
  -- print . length $ findPaths cs "svr" "fft"  -- 5915
  -- print . length $ findPaths cs "dac" "out"  -- 11567
  print $ "Part2 - " <> show (4605696 * 5915 * 11567)
  
findPaths :: Ord a => M.Map a [a] -> a -> a -> [[a]]
findPaths cs from to = 
   let deadEnd (from,to) = unsafePerformIO $ rememberDeadEnd (from,to) >> pure Nothing
       findPaths' path from to 
        | (unsafePerformIO $ isDeadEnd (from,to)) = Nothing
        | from `elem` path = deadEnd (from,to)
        | otherwise =
          let nodes = fromMaybe [] $ M.lookup from cs
            in if null nodes 
               then deadEnd (from,to)
               else if to `elem` nodes 
                    then Just [[from,to]]
                    else 
                      let paths = (concat . catMaybes $ 
                              (\f -> findPaths' (from:path) f to) <$> nodes)
                      in if null . concat $ paths 
                         then deadEnd (from,to)
                         else Just $ (from:) <$> paths
                         
  in fromMaybe [] $ findPaths' [] from to

deadEnds = unsafePerformIO $ newIORef M.empty

rememberDeadEnd fromTo = 
  modifyIORef deadEnds $ (\v -> M.insert fromTo () v)

isDeadEnd fromTo = do 
   mem <- readIORef deadEnds
   pure . isJust $ M.lookup fromTo mem
