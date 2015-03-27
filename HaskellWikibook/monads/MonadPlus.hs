module State where

import Control.Monad
import System.Random
import Control.Monad.Trans.State


digit :: Int -> String -> Maybe Int
digit i s | i > 9 || i < 0 = Nothing
          | otherwise      = do
  let (c:_) = s
  if [c] == show i then Just i else Nothing

binChar :: String -> Maybe Int
binChar s = digit 0 s `mplus` digit 1 s


char :: Char -> String -> Maybe (Char, String)
char c s = do
    let (c':s') = s
    if c == c' then Just (c, s') else Nothing
--char 'C' "Claisse"

--msum  / guard    


{-It would then be possible to write a hexChar function 
which parses any valid hexidecimal character (0-9 or a-f). 
(hint: map digit [0..9] :: [String -> Maybe Int]).-}

hexCharParsers :: [String -> Maybe (Char, String)]
hexCharParsers = map (char . head . show) [0..9]

hexCharsParser :: String -> Maybe (Char, String)
hexCharsParser s = msum (map ($ s) hexCharParsers)
--hexCharsParser "0ab"