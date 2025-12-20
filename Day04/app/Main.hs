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
            [if (atGrid grid (l,r) == 1) && (howManyContainersAround grid (l, r) < 4)
                then Just (l, r) 
                else Nothing 
            | l <- [0..length grid - 1], r <- [0..length (grid !! 0) - 1]]
    print $ "Part1 - " <> show (length result)
    let loop grid sum = do
            let result = catMaybes $
                    [if (atGrid grid (l,r) == 1) && (howManyContainersAround grid (l, r) < 4)
                        then Just (l, r) 
                        else Nothing 
                    | l <- [0..length grid - 1], r <- [0..length (grid !! 0) - 1]]
            if not . null $ result 
            then 
                let newGrid = foldl (\g (l, r) -> update g (l, r) 0) grid result
                in loop newGrid (sum + length result)
            else
                print $ "Part2 - " ++ show sum
    loop grid 0

howManyContainersAround :: Grid -> (Int, Int) -> Int 
howManyContainersAround grid (line, row) = 
    let deltas = [(-1,-1), (-1,0), (-1,1),
                  (0, -1),          (0, 1),
                  (1, -1), (1, 0),  (1, 1)]
    in sum [atGrid grid (line + dl, row + dr) | (dl, dr) <- deltas]

atGrid :: Grid -> (Int, Int) -> Int
atGrid grid (l, r) = 
            if l < 0 || r < 0 || l >= length grid || r >= length (grid !! 0)
            then 0
            else grid !! l !! r

update grid (l, r) v = 
    take l grid ++
    [setAt r v (grid !! l)] ++
    drop (l + 1) grid

setAt :: Int -> a -> [a] -> [a]
setAt idx val xs = take idx xs ++ [val] ++ drop (idx + 1) xs