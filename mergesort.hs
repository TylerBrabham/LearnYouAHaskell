mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge sortedLeft sortedRight
  where 
    (lefts, rights) = splitAt n xs
    sortedLeft = mergesort lefts
    sortedRight = mergesort rights
    n =  length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = [x] ++ merge xs (y : ys)
  | otherwise = [y] ++ merge (x : xs) ys

main = do
  print $ mergesort [1, 5, 4, 6, 7, 4, 8, 9, 3, 2, 6, 1, 5 ,4]