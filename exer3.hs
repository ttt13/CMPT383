import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge (x) [] = x
merge [] (x) = x
merge (x:xs) (y:ys)
 | x < y = x : (merge xs (y:ys))
 | otherwise = y : (merge (x:xs) ys)

--mergesort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = merge (mergeSort firstHalf) (mergeSort secondHalf)
 where
  firstHalf = take halfL x
  secondHalf = drop halfL x
  halfL = div (length x) 2

daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
 where
  jan1 = fromGregorian y 1 1
  dec31 = fromGregorian y 12 31

isFriday :: Day -> Bool
isFriday day
 | snd getDayNumber == 5 = True 
 | otherwise = False
 where 
  getDayNumber = sundayStartWeek day

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

getDay (y,m,d) = d

isPrimeDay :: Day -> Bool
isPrimeDay day
 | divisors getAndConvertDay == [] = True
 | otherwise = False
 where
  getAndConvertDay = getDay (toGregorian day)

primeFridays :: Integer -> [Day]
primeFridays a = [i | i <- (daysInYear a), isFriday i, isPrimeDay i]