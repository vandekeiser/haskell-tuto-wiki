module Monads where

import Control.Monad

rreturn :: a -> Maybe a
rreturn x = Just x

(>>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>>=) a f = case a of 
    Nothing -> Nothing
    Just(x) -> f x
--OK
--(>>>=) (Just(10)) (\ x -> Just(x * 2))      
--OK
--(>>>=) (Just(10)) (Just . (\ x -> x * 2))   
--OK
--(>>>=) (Just(10)) (Just . (*2))
--OK
--Just(10) >>>= (Just . (*2))
--OK
--Just(10) >>>= (rreturn . (*2))

--safeLog :: Double -> Maybe Double
safeLog, safeSqrt, safeLogOfSqrt :: (Floating a, Ord a) => a -> Maybe a
safeLog x 
    | x > 0     = Just(log x)
    | otherwise = Nothing
safeSqrt x 
    | x > 0     = Just(sqrt x)
    | otherwise = Nothing    
{-(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
We can also flip monad composition to go the other direction using (<=<). 
The operation order of (f . g) is the same as (f' <=< g')-}
safeLogOfSqrt = safeLog <=< safeSqrt 
--safeLogOfSqrt 100 = 2.3..

phoneBook :: [(String, String)]
phoneBook = [("Alice", "01 11 11 11 11"), ("Bob", "02 22 22 22 22")]

--lookup :: Eq a => [(a, b)] -> a -> Maybe b
getPhone :: [(String, String)] -> String -> Maybe String
getPhone book name = case [ p | (n, p) <- book, n==name ] of
    []    -> Nothing
    (p:_) -> Just p
getPhoneInBook :: String -> Maybe String
getPhoneInBook = getPhone phoneBook
--getPhoneInBook "Alice"
--getPhoneInBook "Charlie"