
allPrimesHelper :: [Int] -> [Int]
allPrimesHelper (p : xs) =  p :  allPrimesHelper [x | x <- xs, x `mod` p /= 0]

allPrimes :: [Int]
allPrimes = allPrimesHelper [2..]