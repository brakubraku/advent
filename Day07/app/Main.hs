{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.State (State, get, modify, runState)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (deepseq)
import Data.List (nub, (\\), find, findIndex)

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
import Data.Time
import Control.Arrow (ArrowChoice(right))
import Control.Concurrent (modifyMVar, MVar, readMVar, newMVar)

data PathElement = Empty | Splitter | Beam deriving (Eq, Show)

newtype Manifold = Manifold [[PathElement]] deriving (Show)

type Beams = [PathElement] -- positions of beams (x coordinates)

type CountOfSplits = Int -- number of splits occurred

type Level = Int -- current level (y coordinate)

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

type ManifoldMap = M.Map (Int, Int) PathElement

quantumFast :: Manifold -> (Level, Int) -> [Int]
quantumFast m@(Manifold grid) (l, beam) 
 | l >= length grid -1 = [1]
 | beam < 0 || beam >= length (grid !! (l+1)) = []
 | otherwise = if grid !! (l + 1) !! beam == Splitter
    then concat [quantumFast m (l + 1, beam - 1), quantumFast m (l + 1, beam + 1)]
    else quantumFast m (l + 1, beam)
    
quantumFastM :: ManifoldMap -> (Level, Int) -> Bool -> [Int]
quantumFastM m (l, beam) isSplit
 | M.lookup (l,0) m == Nothing = [1]
 | M.lookup (l,beam) m == Nothing = []
 | otherwise = 
  if M.lookup (l + 1, beam) m == Just Splitter
  then 
    let left  = quantumFastM m (l + 1, beam - 1) True
        right = quantumFastM m (l + 1, beam + 1) True
    -- in  left `par` right `pseq` (concat [left, right])
    in 
        if not isSplit then 
            left `par` right `pseq` concat [left, right]
            -- parMap rdeepseq concat [left, right] -- left `par` (right `pseq` (left `rdeepseq` (right `rdeepseq` (concat [left, right]))))
        else
            concat [left, right]
  else quantumFastM m (l + 1, beam) isSplit

quantumFastM1 :: ManifoldMap -> (Level, Int) -> Int
quantumFastM1 m (l, beam)
 | M.lookup (l,0) m == Nothing = 1
 | M.lookup (l,beam) m == Nothing = 0
 | otherwise = 
  if M.lookup (l + 1, beam) m == Just Splitter
  then 
    let left  = quantumFastM1 m (l + 1, beam - 1) 
        right = quantumFastM1 m (l + 1, beam + 1) 
    in  left `par` right `pseq` (left + right)
  else quantumFastM1 m (l + 1, beam)

quantumFastM11 :: ManifoldMap -> MVar (M.Map (Level,Int) Int) -> (Level, Int) -> IO Int
quantumFastM11 m memoized (l, beam)
 | M.lookup (l,0) m == Nothing = pure 1
 | M.lookup (l, beam) m == Nothing = pure 0
 | otherwise = 
    do
        precomputed <- M.lookup (l+1, beam) <$> readMVar memoized 
        case precomputed of
            Just v -> pure v
            _ -> 
                if M.lookup (l + 1, beam) m == Just Splitter
                then do
                    left <- quantumFastM11 m memoized (l + 1, beam - 1) 
                    right <- quantumFastM11 m memoized (l + 1, beam + 1) 
                    left `par` right `pseq` remember (left + right)
                else quantumFastM11 m memoized (l + 1, beam) 
   where
    remember :: Int -> IO Int 
    remember v = modifyMVar memoized $ 
        \m -> do 
          let updated = M.insert (l+1, beam) v m
          pure (updated, v)

quantumBeamToNextLevel :: Manifold -> (Level, Beams) -> [(Level, Beams)]
quantumBeamToNextLevel (Manifold grid) (level, beams) =
  let twoLevels = zip3 [0 ..] beams (grid !! (level + 1))
      nextBeams =
        fmap
          ( \(c, lx, nlx) ->
              case (lx, nlx) of
                (Beam, Splitter) -> [c - 1, c + 1]
                (Beam, Empty) -> [c]
                _ -> []
          )
          twoLevels
      -- find all two member lists - those are the split beams
      splitBeams = filter (\l -> length l == 2) nextBeams

      -- you need the original [c] beams as well
      singleBeams = concat $ nextBeams \\ splitBeams
      nextLevels = (level + 1,) <$> reconstitute . nub <$> ((++singleBeams) <$> combinations splitBeams)
   in if level + 1 >= length grid - 1
     then nextLevels
     else concat $ quantumBeamToNextLevel (Manifold grid) <$> nextLevels
  where
    reconstitute beams = fmap (\c -> if c `elem` beams then Beam else Empty) [0 .. length (head grid) - 1]

combinations :: [[Int]] -> [[Int]]
combinations ((x:y:[]):xs) = fmap (x:) (combinations xs) ++ fmap (y:) (combinations xs)
combinations [] = [[]]

main :: IO ()
main = do
  input <- lines <$> readFile "input"
--   input <- lines <$> readFile "input"
--   let input = lines testGrid
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
  let map = concat $  (\(ln, l) -> 
                let line = zip [0..] l 
                in fmap (\(r,v) -> (ln, r, v)) line) <$> zip [0..] grid
  let mfmap = M.fromList $ fmap (\(l,r,v) -> ((l,r), v)) map
  let (_, _, splits) = beamToNextLevel manifold (0, grid !! 0, 0)
--   let timelines = fst <$> (take 1000 $ quantumBeamToNextLevel manifold (0, grid !! 0))
  let timelines = quantumBeamToNextLevel manifold (0, grid !! 0)
  let fast = quantumFast manifold (0, fromJust (findIndex (== Beam) (grid !! 0))) 
  let fastM = quantumFastM mfmap (0, fromJust (findIndex (== Beam) (grid !! 0))) False
  let fastM1 = quantumFastM1 mfmap (0, fromJust (findIndex (== Beam) (grid !! 0)))
  precomputed <- newMVar M.empty 
  now <- getCurrentTime
  result <- quantumFastM11 mfmap precomputed (0, fromJust (findIndex (== Beam) (grid !! 0)))
  print $ result
  now1 <- getCurrentTime
  print $ diffUTCTime now1 now
--   print $ "Part1 - " ++ show splits
--   print $ "Part2 - " ++ show (length timelines)

testGrid = "..S..\n..^..\n.....\n"