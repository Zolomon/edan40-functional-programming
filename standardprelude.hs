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

