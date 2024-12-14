module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found

--findFirst _ {} = NoMatch
--findFirst f (x:xs) =
findFirst f list =doFindFirst f list 0

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool

palindrome s = s == (rvrs s)

rvrs [] = []
rvrs (x:xs) = (rvrs xs) ++ [x] 
