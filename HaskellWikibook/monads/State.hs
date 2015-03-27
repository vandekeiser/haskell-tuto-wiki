module State where

import Control.Monad
import System.Random
import Control.Monad.Trans.State

roll2DiceIO :: IO (Int, Int)
roll2DiceIO = liftM2 (,) (randomRIO (1,6)) (randomRIO (1,6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = sequence $ map (\i -> randomRIO (1,6)) [1..n]

rollNDiceIO2 :: Int -> IO [Int]
rollNDiceIO2 n = mapM (\i -> randomRIO (1,6)) [1..n]

rollNDiceIO3 :: Int -> IO [Int]
rollNDiceIO3 n = replicateM n $ randomRIO (1,6)

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
lessClumsyRollDice :: StdGen -> ((Int, Int), StdGen)
lessClumsyRollDice gen = ((d1, d2), newgen) where
    (d1, gen1)   = randomR(1,6) gen
    (d2, newgen) = randomR(1,6) gen1

((dA1,dA2), genA) = lessClumsyRollDice $ mkStdGen 0
((dB1,dB2), genB) = lessClumsyRollDice genA


--STATE MONAD--
{-return :: a -> State s a
return x = State ( \ st -> (x, st) )

(>>=) :: State s a -> (a -> State s b) -> State s b
processor >>= processorGenerator = State $ \ st -> 
                                   let (x, st') = runState processor st
                                   in runState (processorGenerator x) st'

put newState = State $ \_ -> ((), newState)
get = State $ \st -> (st, st)-}

type GeneratorState = State StdGen

--GeneratorState Int is in essence a StdGen -> (Int, StdGen)
rollDie :: GeneratorState Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value

rollDice :: GeneratorState (Int, Int)
rollDice = liftM2 (,) rollDie rollDie             
--evalState rollDice (mkStdGen 99)

{-Similarly to what was done for rollNDiceIO, implement a function 
rollNDice :: Int -> GeneratorState [Int] 
that, given an integer, returns a list 
with that number of pseudo-random integers between 1 and 6.-}
rollNDice :: Int -> GeneratorState [Int] --State StdGen [Int]
--rollNDice n = sequence $ map (\i -> rollDie) [1..n]
rollNDice n = replicateM n rollDie 

--evalState rollNDice (mkStdGen 99)

rollNDiceXX :: Int -> [Int] 
rollNDiceXX n = evalState (rollNDice n) (mkStdGen 99)


