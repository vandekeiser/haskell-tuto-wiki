module Monads where

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

safeLog, safeLog2 :: Double -> Maybe Double
safeLog x 
    | x <= 0 = Nothing
    | x > 0  = Just(log x)
safeLog2 x = 
    if x <= 0
        then Nothing
        else Just(log x)    
    