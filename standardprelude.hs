maxi :: Ord a => a -> a -> a
maxi x y = if x <= y then y else x

maxi' :: Ord a => a -> a -> a
maxi' x y 
  | x >= y = x    
  | otherwise = y
                
-- Sum of square, list comprehension style
sumsqlc :: Int -> [Int]
sumsqlc n = [x^2 | x <- take n [1..]]

-- Recursive sum of square
sumsqrc :: Int -> [Int]
sumsqrc 0 = []
sumsqrc n = n^2 : sumsqrc (n-1)

-- Reverse recursive sum of square, bad performance
sumsqrcrv :: Int -> [Int]
sumsqrcrv 0 = []
sumsqrcrv n = sumsqrcrv (n-1) ++ [n^2]

-- Sum of square, via mapping
sumsqmp :: Int -> [Int]
sumsqmp n = take n (map (^2) [1..])

-- Calculate the minimal amount of moves for a Tower of Hanoi of size n.
hanoi :: Int -> Int
hanoi n = 2^n-1

-- Calculate smallest factor of a number, 1 exlusive. 
numFactors :: Int -> Int
numFactors n = length [f | f <- [1..n], n `mod` f == 0]

nextFactor :: Int -> Int -> Int
nextFactor k n = head [f | f <- [1..n], n `mod` f == 0, f > k]

smallestFactor :: Int -> Int
smallestFactor n = nextFactor 1 n

-- 2.2.1 Multiplying List Elements
-- Multiply
multiply :: Num a => [a] -> a
multiply [] = 1
multiply (x:xs) = x * multiply xs

