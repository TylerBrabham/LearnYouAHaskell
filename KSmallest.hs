-- Find k smallest elements
kSmallest :: Int -> [Int] -> [Int]
kSmallest 0 _ = []
kSmallest k (x : xs)
  | (length lefts) < k = lefts ++ (kSmallest (k - (length lefts)) rights)
  | (length lefts) == k = lefts
  | otherwise = kSmallest k lefts
      where lefts = [y | y <- xs, y <= x] ++ [x]
            rights = [y | y <- xs, y > x]

main = do
  print $ kSmallest 5 [5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 5, 4, 3,2, 1, 3, 5, 6, 7, 8, 8, 9, 6, 5, 4, 3, 2, 1, 4]
