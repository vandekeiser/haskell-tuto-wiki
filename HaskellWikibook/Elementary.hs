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




