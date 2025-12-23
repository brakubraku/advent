{-# LANGUAGE TupleSections #-}
module Main where

import Data.List (nub, (\\), findIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Concurrent (modifyMVar, MVar, readMVar, newMVar)

data PathElement = Empty | Splitter | Beam deriving (Eq, Show)

newtype Manifold = Manifold [[PathElement]] deriving (Show)

type Beams = [PathElement] -- positions of beams (x coordinates)

type CountOfSplits = Int -- number of splits occurred

type Level = Int -- current level (y coordinate)

type Memoized  = MVar (M.Map (Level,Int) Int)

beamToNextLevel :: Manifold -> (Level, Beams, CountOfSplits) -> (Level, Beams, CountOfSplits)
beamToNextLevel (Manifold grid) (level, beams, splits) =
  let twoLevels = zip3 [0 ..] beams (grid !! (level + 1))
      nextBeamsAndSplits =
        fmap
          ( \(c, lx, nlx) ->
              case (lx, nlx) of
                (Beam, Splitter) -> ([c - 1, c + 1], 1)
                (Beam, Empty) -> ([c], 0)
                _ -> ([], 0)
          )
          twoLevels
      nextLevel = (level + 1, reconstitue . nub . concatMap fst $ nextBeamsAndSplits, splits + sum (fmap snd nextBeamsAndSplits))
   in if level + 1 >= length grid - 1
        then nextLevel
        else beamToNextLevel (Manifold grid) nextLevel
  where
    reconstitue beams = fmap (\c -> if c `elem` beams then Beam else Empty) [0 .. length (head grid) - 1]

quantumBeamTimelines :: Manifold -> Memoized -> (Level, Int) -> IO Int
quantumBeamTimelines m@(Manifold grid) memoized (l, beam) 
 | l >= length grid -1 = pure 1
 | beam < 0 || beam >= length (grid !! (l+1)) = pure 0
 | otherwise = do 
    precomputed <- M.lookup (l+1, beam) <$> readMVar memoized 
    case precomputed of
        Just v -> pure v
        _ -> do
            if grid !! (l + 1) !! beam == Splitter
            then do 
                left <- quantumBeamTimelines m memoized (l + 1, beam - 1)
                right <- quantumBeamTimelines m memoized (l + 1, beam + 1)
                memoized <- modifyMVar memoized $ 
                    \m -> do 
                      let updated = M.insert (l+1, beam) (left + right) m
                      pure (updated, left + right)
                pure $ memoized
            else quantumBeamTimelines m memoized (l + 1, beam)
    
main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let manifold =
        Manifold $
          fmap
            ( \c -> case c of
                'S' -> Beam
                '.' -> Empty
                '^' -> Splitter
            )
            <$> input
  let (Manifold grid) = manifold
  let (_, _, splits) = beamToNextLevel manifold (0, grid !! 0, 0)
  precomputed <- newMVar M.empty 
  result <- quantumBeamTimelines manifold precomputed (0, fromJust (findIndex (== Beam) (grid !! 0)))
  print $ "Part1 - " ++ show splits
  print $ "Part2 - " ++ show result
