
{-# LANGUAGE TypeApplications #-}
module Main where
import Debug.Trace

main :: IO ()
main = do 
 movements <- fmap parse <$> (lines <$> readFile "input")
 print $ "The password is: " <> show (moveWheel 0 50 movements)
 print $ "The new method password is: " <> show (moveWheel1 0 50 movements)

moveWheel :: Integral t => t -> Integer -> [Integer] -> t
moveWheel zeroCount _ [] = zeroCount 
moveWheel zeroCount pos (m:movements) = 
    let newPos = (pos + m) `mod` 100 
        move = 
         case newPos of 
            0 -> moveWheel (zeroCount+1) 
            _ -> moveWheel zeroCount
    in 
        move newPos movements
        
moveWheel1 :: Integer -> Integer -> [Integer] -> Integer
moveWheel1 zeroCount _ [] = zeroCount
moveWheel1 zeroCount pos (m:movements) = 
    let toMove = m - rotations * 100
        rotations = truncate $ (fromIntegral @_ @Float m) / 100 
        fullRotations = abs $ rotations  
        newPos = (pos + toMove) `positiveModulo` 100
        moveCrossedZero = 
             if toMove == 0 || pos == 0
             then 0 
             else 
                if pos + toMove <= 0 || pos + toMove >= 100 
                then 1 
                else 0
        move = 
            moveWheel1 $ zeroCount + moveCrossedZero + fullRotations
    in 
        move newPos movements

parse :: String -> Integer
parse ('L':rest) = - read rest
parse ('R':rest) = read rest
parse _ = error "Whoopsie daisy"

positiveModulo :: Integer -> Integer -> Integer
positiveModulo x m = ((x `mod` m) + m) `mod` m
