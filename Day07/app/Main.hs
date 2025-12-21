module Main where
import Debug.Trace (trace)
import Data.List ((\\))
import Control.Monad.State (State, modify, get, runState)

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let manifold =
        Manifold $
          fmap
            ( \c -> case c of
                'S' -> Start
                '.' -> Empty
                '^' -> Splitter
            )
            <$> input
  let getStartPosition (Manifold grid) =
        let startY = 0
            startX = head [x | x <- [0 .. length (grid !! startY) -1], (grid !! startY) !! x == Start]
         in Position (startX, startY)
  let start = getStartPosition manifold
  print $ "Grid dimensions=" <> show (length (let (Manifold g) = manifold in g), length (let (Manifold g) = manifold in head g))
  print $ "Start position: " ++ show start
--   error "Debugging"
  let beamResults = flip runState [] $ beamTravel manifold (getStartPosition manifold)
  print $ "Part1 - " ++ (show . length $ fst beamResults)

data PathElement = Start | Empty | Splitter deriving (Eq, Show)

newtype Manifold = Manifold [[PathElement]] deriving Show

newtype Position = Position (Int, Int) deriving (Eq, Show)

type Beams = [Int] -- x coordinates of active beams

beamTravel :: Manifold -> Position -> State Beams [Bool]
beamTravel m (Position (x, y))
  | x >= maxX m || x < 0 = trace ("Out of bounds: " ++ show (x, y)) $ pure []
  | y >= maxY m || y < 0 = trace ("Out of bounds: " ++ show (x, y)) $ pure []
  | otherwise = trace ("At position: " ++ show (x, y)) $
      if isSplitter m (x, y)
        then
         do
          let beam1 = (x + 1, y)
              beam2 = (x - 1, y)
          beams <- get
          resultBeam1 <- if fst beam1 `elem` beams || not (beamInGrid m beam1)
            then pure []
            else 
              do
                modify (\l -> fst beam1: (l \\ [x]))
                beamTravel m (Position beam1)
          resultBeam2 <- if fst beam2 `elem` beams || not (beamInGrid m beam2)
            then pure []
            else do
                modify (\l -> fst beam2 : (l \\ [x])) 
                beamTravel m (Position beam2) 
          pure $ [True] ++ resultBeam1 ++ resultBeam2
                -- ++ if fst beam1 `elem` beams || not (beamInGrid m beam1)
                --     then pure []
                --     else beamTravel m (Position beam1) (fst beam1 : (beams \\ [x]))
                -- ++ if fst beam2 `elem` beams || not (beamInGrid m beam2)
                --     then pure []
                --     else beamTravel m (Position beam2) (fst beam2 : (beams \\ [x]))
        else trace ("Travelling from: " ++ show (x, y + 1)) $ 
                beamTravel m (Position (x, y + 1)) -- continues beam
  where
    maxX (Manifold grid) = length (head grid) - 1
    maxY (Manifold grid) = length grid - 1
    isSplitter (Manifold grid) (x, y) = (grid !! y) !! x == Splitter
    beamInGrid m (x, y) = x >= 0 && y >= 0 && x <= maxX m && y <= maxY m