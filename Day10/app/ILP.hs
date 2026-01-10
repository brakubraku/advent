{-# LANGUAGE FlexibleContexts #-}


module ILP where

import Data.LinearProgram
import Data.LinearProgram.GLPK

solveILP :: [[Int]] -> [Int] -> IO Double
solveILP buttons joltage = do
  let lp :: LP [Int] Int
      lp = execLPM $ do
        setDirection Min
        mapM (\b -> varGeq b 0 >> setVarKind b IntVar) buttons
        let switchButtons = (\(ix,j) -> (filter (ix `elem`) buttons, j)) <$> zip [0..] joltage 
            equation (bs,j) = equalTo (linCombination $ zip (repeat 1) bs) j 
        mapM equation switchButtons
        setObjective . linCombination $ zip (repeat 1) buttons
  (code, (Just (result, _))) <- glpSolveVars mipDefaults {msgLev = MsgOff} lp
  pure result
