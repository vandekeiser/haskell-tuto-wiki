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