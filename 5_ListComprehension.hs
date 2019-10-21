-- double list comprehension
pair = [ (x,y) | x <- [1..3], y <-[1..2] ]
-- concat using list comprehension
-- practice correlated subquery
concat' :: [[a]] -> [a]
concat' [] = []
concat' xss = [ x | xs <- xss, x <- xs ]

-- with guard
even0To10 = [ x | x <- [0..10], even x ]

-- factors
factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]
-- is prime number
isPrime :: Int -> Bool
isPrime n = factors n == [1,n] 

-- check if a triplet make a pythagoras triangle
pyth :: (Int,Int,Int) -> Bool
pyth (a,b,c) 
  | (a*a + b*b) == c*c = True
  | otherwise = False


-- generate all the pythagoras triplets
pyths :: Int -> [(Int,Int,Int)]
pyths n
  = [ (x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], pyth (x,y, z) ] 

  -- check if a number equal sum of all its factor
isPerfectNumber :: Int -> Bool
isPerfectNumber n = sum( init (factors n)) == n
-- generate list of perfect numbers
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfectNumber x ]

-- scalarProduct
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum ([ x * y | (x,y) <- zip xs ys ])

main :: IO()
main = do
    print ([ x**2 | x <- [1..3] ])
    print(pair)