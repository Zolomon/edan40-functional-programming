-- 2.1.1
maxi :: Ord a => a -> a -> a
maxi x y = if x <= y then y else x

maxi' :: Ord a => a -> a -> a
maxi' x y 
  | x >= y = x    
  | otherwise = y
                
-- 2.1.2 Sum of square, list comprehension style
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

-- 2.1.3 Calculate the minimal amount of moves for a Tower of Hanoi of size n.
hanoi :: Int -> Int
hanoi n = 2^n-1

-- 2.1.4 Calculate smallest factor of a number, 1 exlusive. 
numFactors :: Int -> Int
numFactors n = length [f | f <- [1..n], n `mod` f == 0]

nextFactor :: Int -> Int -> Int
nextFactor k n = head [f | f <- [1..n], n `mod` f == 0, f > k]

smallestFactor :: Int -> Int
smallestFactor n = nextFactor 1 n

-- 2.1.5 Defining Types
type Year = Int
type Month = Int
type Day = Int
data Date = Date { year :: Int, month :: Int, day :: Int}

daysInMonth :: Month -> Year -> Day
daysInMonth month year = 
  let daysPerMonth = [31, if isLeapYear then 29 else 28,31,30,31,30,31]    
      isLeapYear = year `divisibleBy` 400 || (not $ year `divisibleBy` 100 && year `divisibleBy` 4)
      a `divisibleBy` b = a `mod` b == 0
  in last (take month daysPerMonth)
   

-- 2.2.1 Multiplying List Elements
multiply :: Num a => [a] -> a
multiply xs = foldr (*) 1 xs

-- 2.2.2 substitution
substitute :: Char -> Char -> [Char] -> [Char]
substitute x y str = [if x == c then y else c | c <- str]

-- 2.2.3 Avoiding duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = if x `elem` xs 
                          then xs 
                          else x : removeDuplicates xs

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = if x `elem` xs
                    then True
                    else duplicates xs