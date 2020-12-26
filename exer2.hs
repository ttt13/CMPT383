
--primes and divisors
divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

-- pythagorean triples
-- Note this is done but its order is different than solution
-- aka this one does second tuple then third tuple
pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2, a < b, b < c]

-- join
join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join separator (x:xs) = x ++ separator ++ join separator xs

-- fact non-recursive
fact' x = foldl(*) 1 [1..x]

-- hailTail and friends
hailstone :: Int -> Int
hailstone n
 | even n = div n 2
 | otherwise = 3 * n + 1

hailLen :: Int -> Int
hailLen n = hailTail 0 n
 where
   hailTail a 0 = a
   hailTail a 1 = a
   hailTail a n = hailTail (a + 1) (hailstone n)