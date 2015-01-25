module Basic where

import Data.Char
import Data.List

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

sign2 x
	| x < 0     = -1
	| x > 0     = 1
	| otherwise = 0	     

pts :: Int -> Int 
--Compile pas!!! les espaces sont significatifs??!!
--	pts 1 = 10
{-Oui! Plus bas: 
"Because indentation matters syntactically in Haskell, 
you need to be careful about whether you are using tabs or spaces. 
By far the best solution is to configure your text editor 
to insert two or four spaces in place of tabs. 
If you keep tabs as distinct, at least ensure that your tabs have always the same length, 
or you're likely to run into trouble."-}
pts 1 = 10
pts 2 =  6
pts x
    | x <= 6    = 7 - x
    | otherwise = 0	

myor :: Bool -> Bool -> Bool
myor False False  = False
myor _     _      = True

fst' :: (a, b) -> a
fst' (x, _) = x
snd' :: (a, b) -> b
snd' (_, y) = y

head' :: [a] -> a
head' (h : _) = h
head' []      = error "empty list"
tail' :: [a] -> [a]
tail' (_ : t) = t
tail' []      = error "empty list"


roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = 
    let disc = sqrt(b * b - 4 * a * c)
        two_as = 2 * a
    in ((-b -disc) / two_as, 
        (-b + disc) / two_as)

fib = 1 : 1 : [ u + v | (u, v) <- zip fib (tail fib)]


twiceSquare = twice . square
    where twice x = 2 * x
          square x = x * x

-- import <module> (Prelude importé automatiqt)
-- mvn = cabal, maven central = hackage, javadoc = hoogle
-- :m +Data.List
testpermutations = permutations "Prelude"          

revWords :: String -> String
revWords = unwords . reverse . words

{-- putStrLn "ffffffff"
 putStrLn :: String -> IO ()       action IO qui retourne rien/void/Unit
 :t getLine
 getLine :: IO String
--}

--The final action defines the type of the whole do block
magIkEenBier :: IO ()
magIkEenBier = do
        putStrLn "Welke biere?"
        biere <- getLine
        putStrLn "Halv of pint?"
        vol <- getLine
        putStrLn ("Has u bleeft, uwe " ++ biere ++ " " ++ vol)

triangleArea :: IO ()
triangleArea = do
    putStrLn "Base?"
    base <- getLine --KO de mettre read ici, read attend un String pas un IO
    putStrLn "Height?"
    height <- getLine
    putStrLn ("Area is: " ++ show(read base * read height / 2))

classifyUserWrtHaskell1 :: IO ()
classifyUserWrtHaskell1 = do
    putStrLn "Votre nom?"   
    nom <- getLine
    if nom == "Simon"
        then putStrLn "Gourou"
        else if nom == "John" 
            then putStrLn "Gourou"
            else if nom == "Phil" 
                then putStrLn "Gourou"
                else putStrLn "Pekin lambda"

classifyUserWrtHaskell2 = do
    putStrLn "Votre nom?"   
    nom <- getLine
    putStrLn (classe nom)
    where
        classe "Simon" = "Gourou"
        classe "John"  = "Gourou"
        classe "Phil"  = "Gourou"
        classe _       = "Pekin lambda"

classifyUserWrtHaskell3 = do
    putStrLn "Votre nom?"   
    nom <- getLine
    putStrLn (classe nom)
    where 
        classe n =
            if n=="Simon" || n=="Phil" || n=="John"
                then "Gourou"
                else "Pekin lambda"        


-- do = cree un bloc action
-- action = do keyword