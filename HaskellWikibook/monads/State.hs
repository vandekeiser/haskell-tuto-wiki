module State where

import Control.Monad
import System.Random

roll2DiceIO :: IO (Int, Int)
roll2DiceIO = liftM2 (,) (randomRIO (1,6)) (randomRIO (1,6))