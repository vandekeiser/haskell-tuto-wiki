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

--power x n
power x 0 = 1
power x 1 = x
power x n = x * power x (n - 1)