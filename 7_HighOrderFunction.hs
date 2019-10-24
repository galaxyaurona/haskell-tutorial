-- simplies HoF
twice :: (a -> a) -> a -> a
twice f = f . f

double :: Int -> Int 
double n = n * 2
-- compose quadruple as twice double
quadruple :: Int -> Int
quadruple n = twice double n

-- define map recursively
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map' f xs) 
-- define filter recursviely

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x        = x : filter' f xs 
-- first attemp
---                 | not (f x)  = filter' f xs 
-- second attemp
                 | otherwise  = filter' f xs 

-- define foldr' recursive;y
-- first argument is a function that take value of type a,
--  seed value of type b, and transform that value to final value
-- of type b
-- second argument is seed of type b
-- third is list of type a to apply func
-- return type b
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f x [] = x -- empty list return seed
foldr' f x (y:ys) =  f y (foldr' f x ys)

-- sum as foldr composition

-- = reduce, with b = seed value
sum' :: Num a => [a] -> a
sum' = foldr' (+) 0 

-- product of list
product' :: Num a => [a] -> a
product' = foldr' (*) 1

and' :: [Bool] -> Bool
and' = foldr' (&&) True
-- length as function composition
-- map each element to 1, then foldr over that list
length' :: [a] -> Int
-- using lambda notation with multiple arguments
length' = foldr' (\_ y -> 1 + y ) 0 

-- different function composition
length'' :: [a] -> Int
length'' = sum . (map (\_ -> 1))

-- reverse foldr'
-- is init and concat
reverse' :: [a] -> [a]
reverse' = foldr' (\ x xs -> xs ++ [x]) []    

-- all' using foldr
-- all element satisfied condition
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr' (\x y -> (f x) && y) True

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr' (\x y -> f x || y) False

-- map using foldr'
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr' (\x y -> [f x] ++ y) [] 

-- filter using foldr'
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr' (\x y -> if (f x) then [x] ++ y else y ) []

main = do
    print("Hello")