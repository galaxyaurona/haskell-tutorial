-- conditional expression
abs' :: Int -> Int
abs' n = if n >= 0 then n else -n
-- abs using guard
absguard :: Int -> Int
absguard n  | n < 0 = n
            | otherwise = n
-- another guard
signum' :: Int -> Int
signum' n   | n < 0     = -1
            | n == 0    = 0
            | otherwise = 1

-- pattern matching
xor' :: Bool -> Bool -> Bool
--xor' False False    =   False
--xor' True False     =   True
--xor' False True     =   True
--xor' True True      =   False
-- simplified
xor' False n    = n
xor' True n     = not n 


-- custom and operator
(&&/) :: Bool -> Bool -> Bool
(&&/) True  n = n
(&&/) False _ = False

-- head
head' :: [a] -> a
head' []    = undefined
head' (a:_) = a

-- tail
tail' :: [a] -> [a]
tail' (_:xs) = xs

-- pred
pred :: Int -> Int
pred (n+1) = n
-- add the row belows to allow negative number and 0
pred (n) = n-1

-- 1
-- safetail using condtionalExpression
safetail :: Eq a => [a] -> [a]
safetail n = if n == [] then [] 
             else xs
                where (_:xs) = n

-- safetail using guarded equation
safetail' :: Eq a => [a] -> [a]
safetail' n | n == [] = [] 
            | otherwise = xs where (_:xs) = n
-- safetail pattern matching
safetail'' :: Eq a => [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

-- 2 only show the most efficient one
(|||) :: Bool -> Bool -> Bool
(|||) True  n = True
(|||) False n = n

-- 3
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if (x == True && y == True) then True else False 
--- 4
(&&&&) :: Bool -> Bool -> Bool
(&&&&) x y = if (x == True) then y else False 

--main function
main :: IO()
main = do
    print(absguard (-2))
    print(xor' True False)
    print( True &&/ False )
    print( head'([] :: [Int]))