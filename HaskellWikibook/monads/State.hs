module State where

import Control.Monad
import System.Random

roll2DiceIO :: IO (Int, Int)
roll2DiceIO = liftM2 (,) (randomRIO (1,6)) (randomRIO (1,6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = sequence $ map (\i -> randomRIO (1,6)) [1..n]

rollNDiceIO2 :: Int -> IO [Int]
rollNDiceIO2 n = mapM (\i -> randomRIO (1,6)) [1..n]

clumsyRollDice :: (Int, Int)
clumsyRollDice = (d1, d2) where
    (d1, gen) = randomR(1,6) $ mkStdGen 0
    (d2, _)   = randomR(1,6) gen


{-Implement a function 
rollDice :: StdGen -> ((Int, Int), StdGen) 
that, given a generator, 
return a tuple 
with our random numbers as first element 
and the last generator as the second.    -}
rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice gen = ((d1, d2), newgen) where
    (d1, gen1)   = randomR(1,6) gen
    (d2, newgen) = randomR(1,6) gen1

((dA1,dA2), genA) = rollDice $ mkStdGen 0
((dB1,dB2), genB) = rollDice genA

