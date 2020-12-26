import Data.Ratio
import Data.List 


myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- mySplitAt :: Int -> [a] -> ([a], [a])
-- mySplitAt _ [] = ([], [])
-- mySplitAt a x = (take a x, drop a x)

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt a x = recSplit a [] x
 where
  recSplit _ _ [] = ([], [])
  recSplit 0 x y = (x, y)
  recSplit a x (y:ys) = recSplit (a-1) (x ++ [y]) ys


rationalSum :: Integer -> [Ratio Integer]
rationalSum a = [i % j | i <- [1..a], j <- [1..a], i + j == a]

rationalSumLowest :: Integer -> [Ratio Integer]
rationalSumLowest a = [i % (a - i) | i <- [1..a], gcd i (a - i) == 1]

-- Received help from Stack Overflow on how to turn list of lists into a list
-- Please refer to: https://stackoverflow.com/questions/12788514/haskell-turning-list-of-lists-into-one-list
rationals :: [Ratio Integer]
rationals = concat [rationalSumLowest i | i <- [2..]]

-- split a list around a given separator value
splitAtSeparator :: Eq a => a -> [a] -> [[a]]
splitAtSeparator sep [] = []
splitAtSeparator sep content = first : splitAtSeparator sep rest
    where
    first = takeWhile (/= sep) content
    firstlen = length first
    rest = drop (firstlen+1) content

-- convert an integer-like string to an integer
readInt :: String -> Int
readInt = read

sumFile :: IO()
sumFile = do
 line <- readFile "input.txt" 
 let 
  parseLine = splitAtSeparator '\n' line
  convertInt count [] = count
  convertInt count (x:xs) = convertInt (count + (readInt x)) xs
  getSum = convertInt 0 parseLine
 print getSum


