module Main where
import Data.Maybe (catMaybes)

type Grid = [[Int]] -- 0 = empty, 1 = container

-- Small test grid for testing purposes (values only 0 and 1)
-- Larger 10x10 test grid for testing purposes (values only 0 and 1)
testGrid :: Grid
testGrid =
    [ [0,1,0,1,0,1,0,1,0,1]
    , [1,0,1,0,1,0,1,0,1,0]
    , [0,1,1,1,0,0,1,1,0,0]
    , [1,0,0,0,1,1,0,0,1,1]
    , [0,1,0,1,0,1,0,1,0,1]
    , [1,0,1,0,1,0,1,0,1,0]
    , [0,0,1,1,0,0,1,1,0,0]
    , [1,1,0,0,1,1,0,0,1,1]
    , [0,1,0,1,0,1,0,1,0,1]
    , [1,0,1,0,1,0,1,0,1,0]
    ]

main :: IO ()
main = do 
    gridLinesString <- lines <$> readFile "input"
    let convertLine = fmap 
         (\c -> case c of
            '.' -> 0
            '@' -> 1)
    let grid = convertLine <$> gridLinesString
    let result = catMaybes $
            [if (atGrid (l,r) grid == 1) && (howManyContainersAround grid (l, r) < 4)
                then Just (l, r) 
                else Nothing 
            | l <- [0..length grid - 1], r <- [0..length (grid !! 0) - 1]]
    print $ length result

howManyContainersAround :: Grid -> (Int, Int) -> Int 
howManyContainersAround grid (line, row) = 
    let deltas = [(-1,-1), (-1,0), (-1,1),
                  (0, -1),          (0, 1),
                  (1, -1), (1, 0),  (1, 1)]
    in sum [atGrid (line + dl, row + dr) grid | (dl, dr) <- deltas]

atGrid :: (Int, Int) -> Grid -> Int
atGrid (l, r) grid = 
            if l < 0 || r < 0 || l >= length grid || r >= length (grid !! 0)
            then 0
            else grid !! l !! r



