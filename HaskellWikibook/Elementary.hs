module Elementary where

import Data.List

fact1 n = 
    if n < 0 then error "cannot be < 0"
    else if n == 0 then 1
    else n * fact1 (n-1)

fact2 0 = 1
fact2 n = 
    if n < 0 then error "cannot be < 0"
    else n * fact2 (n - 1)

doublefact 0 = 1
doublefact 1 = 1
doublefact n =
    if n < 0 then error "cannot be < 0"
    else n * doublefact (n - 2)

doublefact2 n
    | n < 0 = error "cannot be < 0"
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = n * doublefact2 (n - 2)

-- VV traduction iteration + mutation ->recurs
factByAcc n = acc n 1
    where
    acc cnt res
        | cnt <= 1   = res
        | otherwise  = acc (cnt - 1) (cnt * res)

factByAcc2 n = acc n 1 where
    acc cnt res
        | cnt <= 1   = res
        | otherwise  = acc (cnt - 1) (cnt * res)        

mult _ 0 = 0         
mult x 1 = x
mult x y = x + (mult x (y - 1))

power x 0 = 1
power x 1 = x
power x n = x * power x (n - 1)

plusOne x = x + 1
addition x 0 = x
addition x y = plusOne (addition x (y - 1))

{-(Harder) Implement the function log2, 
which computes the integer log (base 2) of its argument. 
That is, log2 computes 
the exponent of the largest power of 2 
which is less than or equal to its argument. 
For example, log2 16 = 4, log2 11 = 3, and log2 1 = 0.-}
--             2^  4 =16,   2^  3 = 8,       2^ 0 = 1 
log2 :: Int -> Int --pour etre sur a la compil que la division par 2 est entiere
log2 1 = 0
log2 2 = 1
--log2 n = 1 + log2 (/ n 2) --KO / donne un Fractional
log2 n = 1 + log2 (div n 2) 

length2 :: [a] -> Int
length2 []      = 0
length2 (elt : rest) = 1 + (length2 rest)

listconcat :: [a] -> [a] -> [a]
listconcat [] l2  = l2
--listconcat l1 l2  = (head l1) : listconcat (tail l1) l2
listconcat (e:l1) l2  = e : listconcat l1 l2

{-replicate :: Int -> a -> [a], 
which takes a count and an element 
and returns the list which is that element repeated that many times. 
E.g. replicate 3 'a' = "aaa".-}
repl :: Int -> x -> [x]
repl 0 elt = []
repl n elt = elt : repl (n-1) elt

{-(!!) :: [a] -> Int -> a, 
which returns the element at the given 'index'. -}
get :: [a] -> Int -> a
get []    _ = error "no such element"
get (h:t) 0 = h
get (h:t) n = get t (n-1)

{-zip :: [a] -> [b] -> [(a, b)], 
which takes two lists and 'zips' them together, 
E.g. zip [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]. 
If either of the lists is shorter than the other, you can stop once either list runs out.-}
zzip :: [a] -> [b] -> [(a,b)]
zzip _       []      = []
zzip []      _       = []
zzip (h1:t1) (h2:t2) = (h1, h2) : (zzip t1 t2)

length3 :: [a] -> Int
length3 l = acc 0 l
    where
        acc n []    = n 
        acc n (h:t) = acc (n+1) t

double :: Num(a) => [a] -> [a]
double [] = []
double (h:t) = 2*h : (double t)

mul :: Num a => a -> [a] -> [a]
mul _ []    = []
mul k (h:t) = (k*h) : (mul k t)

{-takeInt returns the first n items in a list. 
So takeInt 4 [11,21,31,41,51,61] returns [11,21,31,41]-}
ttake :: Int -> [a] -> [a]
ttake 0 _    = []
ttake n (h:t) = h : (ttake (n-1) t)
ttake _ []    = error ("no such element")

{-dropInt drops the first n items in a list and returns the rest. 
so dropInt 3 [11,21,31,41,51] returns [41,51].-}
ddrop :: Int -> [a] -> [a]
ddrop 0 liste = liste
ddrop n (h:t) = ddrop (n-1) t
ddrop _ []    = error ("no such element")

{-sumInt returns the sum of the items in a list.-}
ssum :: Num a => [a] -> a
ssum (h:t) = h + ssum (t)
ssum []    = 0

{-scanSum adds the items in a list and returns a list of the running totals. 
So scanSum [2,3,4,5] returns [2,5,9,14]-}
scanSum :: Num a => [a] -> [a]
scanSum l = acc 0 l
    where        
        acc _ []    = []
        acc s (h:t) = (h+s) : acc (h+s) t
--sans accumulation:
scanSum2 :: Num a => [a] -> [a]
scanSum2 []            = []
scanSum2 (h1 : [])     = h1 : []
scanSum2 (h1 : h2 : t) = h1 : scanSum2((h1 + h2) : t)

{-diffs returns a list of the differences between adjacent items. 
So diffs [3,5,6,8] returns [2,1,2]-}
diffs :: Num a => [a] -> [a]
diffs []            = []
diffs (h1 : [])     = []
diffs (h1 : h2 : t) = (h2 - h1) : diffs(h2 : t)

ddouble :: Int -> Int
ddouble x = 2 * x
--on peut inliner: mmap (10*) [1,2,3]=[10,20,30]
mmap :: (Int->Int)->[Int]->[Int]
mmap _ []      = []
mmap f (h : t) = (f h) : (mmap f t)

{-Use map to build functions that, given a list xs of Ints, return:
    -A list that is the element-wise negation of xs-}
moins :: [Int] -> [Int]
moins = map neg where neg x = (0-x)

{-A list of lists of Ints xss that, for each element of xs, contains the divisors of xs. 
You can use the following function to get the divisors:
divisors p = [ f | f <- [1..p], p `mod` f == 0 ]-}
divs :: [Int] -> [[Int]]
divs = map divisors where divisors p = [ f | f <- [1..p], p `mod` f == 0 ]

{-Implement a Run Length Encoding (RLE) encoder and decoder.
    The idea of RLE is simple; given some input:
    "aaaabbaaa"
    compress it by taking the length of each run of characters:
    (4,'a'), (2, 'b'), (3, 'a')
    The concat and group functions might be helpful. 
    In order to use group, you will need to import the Data.List module. -}
rle :: [Char] -> [(Int, Char)]    
rle str = acc str []
    where
        acc :: [Char] -> [(Int, Char)] -> [(Int, Char)]
        acc []       rrle             = rrle
        acc (c : []) []               = (1, c) : []
         {-Couldn't match expected type `[(Int, Char)]'
             with actual type `(t0, Char)'
             In the second argument of `acc', namely `(1, hstr)'
             In the first argument of `(:)', namely `(acc tstr (1, hstr))'
             In the expression: (acc tstr (1, hstr)) : []-}
        acc (hstr : tstr) []          = acc tstr ((1, hstr):[])
        acc (hstr : tstr) (hrle:trle) =
            if (hstr == (snd hrle)) then ((fst hrle)+1, hstr) : (acc tstr trle)
            else                         (           1, hstr) : (acc tstr (hrle:trle))
