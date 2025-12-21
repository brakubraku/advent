module Main where

import Control.Monad.State (State, get, modify, runState)
import Data.List (nub, (\\))
import Debug.Trace (trace)

data PathElement = Empty | Splitter | Beam deriving (Eq, Show)

newtype Manifold = Manifold [[PathElement]] deriving (Show)

type Beams = [PathElement] -- positions of beams (x coordinates)

type CountOfSplits = Int -- number of splits occurred

type Level = Int -- current level (y coordinate)

beamToNextLevel :: Manifold -> (Level, Beams, CountOfSplits) -> (Level, Beams, CountOfSplits)
beamToNextLevel (Manifold grid) (level, beams, splits) =
  let twoLevels = zip3 [0 ..] beams (grid !! (level + 1))
      next =
        fmap
          ( \(c, lx, nlx) ->
              case (lx, nlx) of
                (Beam, Splitter) -> ([c - 1, c + 1], 1)
                (Beam, Empty) -> ([c], 0)
                _ -> ([], 0)
          )
          twoLevels
      nextLevel = (level + 1, reconstitue . nub . concatMap fst $ next, splits + sum (fmap snd next))
   in if level + 1 >= length grid - 1
        then nextLevel
        else beamToNextLevel (Manifold grid) nextLevel
  where
    reconstitue beams = fmap (\c -> if c `elem` beams then Beam else Empty) [0 .. length (head grid) - 1]

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
  print $ "Part1 - " ++ show splits
