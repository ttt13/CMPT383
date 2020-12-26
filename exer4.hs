import Data.Maybe

pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = [1] ++ zipWith (+) allButTail allButHead ++ [1]
 where 
  prev = pascal (n-1)
  allButTail = init prev
  allButHead = tail prev

addPair :: Num a => (a, a) -> a
addPair = (uncurry(+))

withoutZeros :: (Num a, Eq a) => [a] -> [a]
withoutZeros = filter (/= 0)

findElt :: Eq a => a -> [a] -> Maybe Int
findElt _ [] = Nothing
findElt a x = fIndex 0 a x
 where
  fIndex _ _ [] = Nothing
  fIndex counter a (x:xs)
   | a == x = Just counter
   | otherwise = fIndex (counter + 1) a xs
