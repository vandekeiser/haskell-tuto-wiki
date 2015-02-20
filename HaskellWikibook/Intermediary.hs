module Intermediary where

doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  case compare (read guess) num of
    LT -> do putStrLn "Too low!"
             doGuessing num
    GT -> do putStrLn "Too high!"
             doGuessing num
    EQ -> putStrLn "You Win!"

doGuessingBraces num = do {
    putStrLn "Enter your guess:"; guess <- getLine;
    case compare (read guess) num of {
        LT -> do { putStrLn "Too low!" ;  doGuessing num; };
        GT -> do { putStrLn "Too high!"; doGuessing num; };
        EQ -> putStrLn "You Win!";
    }
}
    
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f = g where
  g (Leaf x) = Leaf (f x)
  g (Branch left right) = Branch (g left) (g right)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fbranch fleaf = g where
  g (Leaf x) = fleaf x
  g (Branch left right) = fbranch (g left) (g right)

tree1 :: Tree Integer
tree1 = 
    Branch
       (Branch 
           (Branch 
               (Leaf 1) 
               (Branch (Leaf 2) (Leaf 3))) 
           (Branch 
               (Leaf 4) 
               (Branch (Leaf 5) (Leaf 6)))) 
       (Branch
           (Branch (Leaf 7) (Leaf 8)) 
           (Leaf 9))
 
doubleTree = treeMap  (*2)         -- doubles each value in tree
sumTree    = treeFold (+)  id      -- sum of the leaf values in tree
fringeTree = treeFold (++) (: [])  -- list of the leaves of tree
{-
doubleTree tree1
sumTree tree1
fringeTree tree1
-}





data Weird a b = First a
               | Second b
               | Third [(a,b)]
               | Fourth (Weird a b)

weirdMap :: (a->c) -> (b->d) -> Weird a b -> Weird c d
weirdMap fa fb = g where
    g (First a)      = First (fa a)
    g (Second b)     = Second (fb b)
    g (Third abs) = Third ([(fa a, fb b) | (a, b) <- abs])
    g (Fourth w) = Fourth (g w)
    
weirdFold :: (a->r) -> (b->r) -> ([(a, b)]->r) -> (r->r) -> Weird a b -> r
weirdFold fa fb ft fw = g where
    g (First a)  = fa a
    g (Second b) = fb b
    g (Third t)  = ft t
    g (Fourth w) = fw (g w)




