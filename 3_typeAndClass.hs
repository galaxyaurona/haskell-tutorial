
-- currying
add' :: Num a => a -> a -> a
add' x y = x + y
-- currying
add2 :: Num a => a -> a
add2 =  add' 2

-- implement take using recursion
take' :: Int -> [a] -> [a]
take' 0 _    = []
take' _ []   = []
take' n (x:xs)  = (x : take' (n-1) xs)

-- implement zip'
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
-- take out first pair and zip the rest
zip' (x:xs) (y:ys) = ((x,y): zip' xs ys )

listOfFunc = [init,tail,reverse,tail]
-- test functiong
main = do
    print( add' 2 3)
    print( add2 5)
    print( zip' [1,2,3,4] [0,1])
    -- check types
    -- :t listOfFunc = [[a]-> [a]]