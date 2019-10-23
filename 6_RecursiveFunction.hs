-- recursive product
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs  
-- factorial based on product
factorial :: Int -> Int
factorial n = product' (take n [1..n])
-- recusive factorial
factorial' :: Int -> Int
factorial' 0 = 1
-- without n+k pattern negative will fail
factorial' (n+1) = (n+1) * factorial n 
-- recursive factorial = recursive product

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = ((reverse' xs) ++ [x])

zip' :: [a] -> [b] -> [(a,b)]
zip' _ []   = []
zip' [] _   = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs      = xs
drop' _ []      = []
drop' n (x:xs)  = drop' (n-1) xs

(+++) :: [a] -> [a] -> [a]
(+++) [] xs = xs
(+++) xs [] = xs
(+++) (x:xs) ys = x : (xs+++ys)

and' :: [Bool] -> Bool
and' []         = True
and' (False: xs) = False
and' (x:xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs +++ (concat' xss)

replicate' :: Int -> a -> [a]
replicate' 0 _  = []
replicate' n x = x : (replicate' (n-1) x)

(!!!) :: [a] -> Int -> a
(!!!) [] _      = undefined
(!!!) (x:xs) 0  = x
(!!!) (x:xs) n = xs !!! (n-1)

elem' :: Eq a =>  a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem x ys

main :: IO()
main = do
    print("Hello")
