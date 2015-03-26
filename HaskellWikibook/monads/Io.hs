module List where

import Control.Monad
import Data.Char (toUpper)
 
--greetAndSeeYou :: IO ()
-- Reminder:     liftM f m == m >>= return . f == fmap f m
--f == seeYou == (String -> String)
--m == nameReturn == IO String
--liftM f m = nameReturn(IO String) >>= return . seeYou(String -> String)
--liftM f m = nameReturn(IO String) >>= returnSeeYou(String -> IO String)
--liftM f m = X (IO String)
--greetAndSeeYou = liftM seeYou nameReturn >>= putStrLn


{-Ask the user to insert a string
Read their string
Use liftM to apply a function shout that capitalizes all the letters from the string
Write the resulting string-}
shout :: String -> String
shout x = map toUpper x ++ "!!!"

getShoutee :: IO String
getShoutee = do
    putStrLn "What do you want me to shout?"  
    getLine

--liftM shout m  ==  m >>= return . shout
askAndShout, askAndShout2 :: IO()
askAndShout = liftM shout getShoutee >>= putStrLn
askAndShout2 = putStrLn "What do you want me to shout?" 
                >> liftM shout getLine 
                >>= putStrLn
--OK aussi:
--askAndShout2 = putStrLn "What do you want me to shout?" >> getLine >>= return . shout >>= putStrLn

printList :: Show a => [a] -> IO ()
printList xs = forM_ xs (putStrLn . show)


printBunnyGen :: Int -> IO ()    
printBunnyGen n = forM_ (bunnyGen n) putStrLn where 
    bunnyGen 1 = ["bunny"]
    bunnyGen n = (bunnyGen (n-1)) >>= generation
    generation = replicate 3

--sequence [Just 1, Just 2, Just 3]    
--Just [1, 2, 3]
--sequence [Just 1, Nothing]    
--Nothing