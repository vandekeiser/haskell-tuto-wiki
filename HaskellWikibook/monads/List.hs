module List where

import Control.Monad

rreturn :: a -> [a]
rreturn x = [x]

(>>>=) :: [a] -> (a -> [b]) -> [b]
(>>>=) xs f = concat (map f xs)
--[1, 2, 3] >>>= \x->[0..x]     == [0,1,0,1,2,0,1,2,3]


 
--greetAndSeeYou :: IO ()
-- Reminder:     liftM f m == m >>= return . f == fmap f m
--f == seeYou == (String -> String)
--m == nameReturn == IO String
--liftM f m = nameReturn(IO String) >>= return . seeYou(String -> String)
--liftM f m = nameReturn(IO String) >>= returnSeeYou(String -> IO String)
--liftM f m = X (IO String)
--greetAndSeeYou = liftM seeYou nameReturn >>= putStrLn


generation = replicate 3
bunnyGeneration = ["bunny"] >>= generation >>= generation
--["bunny","bunny","bunny","bunny","bunny","bunny","bunny","bunny","bunny"]-}
