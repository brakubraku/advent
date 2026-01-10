{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Data.Function ((&))

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

type Connections = M.Map T.Text [T.Text]

findPaths :: Connections -> T.Text -> T.Text -> [[T.Text]]
findPaths cs from to = 
  let findPaths' :: T.Text -> T.Text -> Maybe [[T.Text]]
      findPaths' from to = 
        let nodes = fromMaybe [] $ M.lookup from cs
          in if null nodes 
             then Nothing 
             else if to `elem` nodes 
                  then Just [[to]]
                  else Just $
                     (from:) <$> (concat . catMaybes $ 
                                    flip findPaths' to <$> nodes)
  in fromMaybe [] $ findPaths' from to

