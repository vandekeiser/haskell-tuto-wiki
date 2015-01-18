module Toto where

import Data.Char

--comm 1
area r = {-comm2-} pi * r ^ 2

--Define a function that subtracts 12 from half its argument.
sub12fromhalf x = (x / 2) - 12

rectarea l w = l * w

sqarea s = rectarea s s

--Write a function to calculate the volume of a box.
vol l w h = l * w * h

-- Heron's formula: sqrt{s(s-a)(s-b)(s-c)}, s=1/2 a+b+c
heron a b c = sqrt(s * (s-a) * (s-b) * (s-c))
	where s = (a + b + c) / 2

abs x
	| x < 0     = -x
	| otherwise = x
--abs -5 KO, abs (-5) OK

nbRealRoots a b c
	| delta <  0 = 0
	| delta == 0 = 1
	| delta >  0 = 2
	where delta = b^2-4*a*c
--1+x^2 pas de racine reelle	
--1-x^2 2 racines reelles

xor :: Bool -> Bool -> Bool --optionnel (type inference)
xor p q = (p || q) && not (p && q)

--import Data.Char
uppercase, lowercase :: String -> String
uppercase = map toUpper
lowercase = map toLower

shoutHello :: String -> String
shoutHello x = uppercase ("hello " ++ x ++ "!!!")

{-Write a function cons8 that takes a list and conses 8 (at the beginning) on to it. 
Test it out on the following lists by doing:
    cons8 []
    cons8 [1,2,3]
    cons8 [True,False]
    let foo = cons8 [1,2,3]
    cons8 foo-}
--cons8 :: Num a => a -> [a] -> [a] KO
--elt : list 
cons8 as = 8 : as
--list1 ++ list 2
rcons8 as = as ++ [8]

listOfListsOfLists = ([]:[]) : []

headtail :: [a] -> (a, [a])
headtail list = (f, l)
	-- !!fst/snd prennent des tuples pas des listes
	where f = head list
	      l = tail list

sec list     = x
	where x  = head x2
	      x2 = tail list	      

fifth :: [a] -> a
fifth list   = head x1
	where x1 = tail x2
	      x2 = tail x3
	      x3 = tail x4
	      x4 = tail list

h :: Int -> a -> b -> Char
h x y z = chr (x - 2)	      

somme :: (Num a) => a -> a -> a
somme x y = x + y

{-When a Haskell program evaluates (-7) + 5.12, 
it must settle for an actual type for the numbers. 
It does so by performing type inference while accounting for the class specifications. 
(-7) can be any Num, 
but there are extra restrictions for 5.12, so its type will define what (-7) will become. 
Since there is no other clues to what the types should be, 
5.12 will assume the default Fractional type, which is Double-}
	-- ?Donc Fractional est un "type abstrait" ou qqch ds le genre 
	-- puisqu'il ne peut pas etre instancié?
	-- On peut pas lui dire qu'on veut une impl sans perte de précision?


--heterogene = 4 / length [1, 2, 3]	--No instance for (Fractional Int) arising from a use of `/'
-- The result of length is not a polymorphic constant, but an Int; 
-- and since an Int is not a Fractional it can't fit the signature of (/).

--fromIntegral :: (Integral a, Num b) => a -> b
heterogene = 4 / fromIntegral (length [1, 2, 3])


--Cas different du precedent:
--(==) :: (Eq a) => a -> a -> Bool
egal = (==) 3 (length [1, 2, 3])
--3 est un Eq, length [1, 2, 3] est un Int
-- Int n'est tjrs pas polymorphique mais c'est un Eq 
-- (dans le cas précédent Int n'etait pas un Fractional)